import { Span } from "./error";
import { LitIntType } from "./lexer";

export type Ast = {
  rootItems: Item[];
  typeckResults?: TypeckResults;
  itemsById: Map<ItemId, Item>;
  packageName: string;
};

export type Identifier = {
  name: string;
  span: Span;
  res?: Resolution;
};

export type ItemId = number;

export type ItemKind =
  | {
      kind: "function";
      node: FunctionDef;
    }
  | {
      kind: "type";
      node: TypeDef;
    }
  | {
      kind: "import";
      node: ImportDef;
    }
  | {
      kind: "mod";
      node: ModItem;
    };

export type Item = ItemKind & {
  span: Span;
  id: ItemId;
  defPath?: string[];
};

export type FunctionDef = {
  name: string;
  params: FunctionArg[];
  body: Expr;
  returnType?: Type;
  ty?: TyFn;
};

export type FunctionArg = {
  name: string;
  type: Type;
  span: Span;
};

export type TypeDef = {
  name: string;
  fields: FieldDef[];
  ty?: TyStruct;
};

export type FieldDef = {
  name: Identifier;
  type: Type;
};

export type ImportDef = {
  module: StringLiteral;
  func: StringLiteral;
  name: string;
  params: FunctionArg[];
  returnType?: Type;
  ty?: TyFn;
};

export type ModItem = {
  name: string;
  modKind: ModItemKind;
};

export type ModItemKind =
  | {
      kind: "inline";
      contents: Item[];
    }
  | {
      kind: "extern";
    };

export type ExprEmpty = { kind: "empty" };

export type ExprLet = {
  kind: "let";
  name: Identifier;
  type?: Type;
  rhs: Expr;
  // IMPORTANT: This is (sadly) shared with ExprBlock.
  // TODO: Stop this sharing and just store the stack of blocks in typeck.
  local?: LocalInfo;
};

// A bit like ExprBinary except there are restrictions
// on the LHS and precedence is unrestricted.
export type ExprAssign = {
  kind: "assign";
  lhs: Expr;
  rhs: Expr;
};

export type ExprBlock = {
  kind: "block";
  exprs: Expr[];
  // IMPORTANT: This is (sadly) shared with ExprLet.
  locals?: LocalInfo[];
};

export type ExprLiteral = {
  kind: "literal";
  value: Literal;
};

export type ExprIdent = {
  kind: "ident";
  value: Identifier;
};

/**
 * `a.b.c` in source code where `a` and `b` are modules.
 * This expression is not parsed, but fieldAccess gets converted
 * to path expressions in resolve.
 */
export type ExprPath = {
  kind: "path";
  segments: string[];
  /**
   * Since this only exists after resolve, we always have a res.
   */
  res: Resolution;
};

export type ExprBinary = {
  kind: "binary";
  binaryKind: BinaryKind;
  lhs: Expr;
  rhs: Expr;
};

export type ExprUnary = {
  kind: "unary";
  unaryKind: UnaryKind;
  rhs: Expr;
};

export type ExprCall = {
  kind: "call";
  lhs: Expr;
  args: Expr[];
};

export type ExprFieldAccess = {
  kind: "fieldAccess";
  lhs: Expr;
  field: {
    value: string | number;
    span: Span;
    fieldIdx?: number;
  };
};

export type ExprIf = {
  kind: "if";
  cond: Expr;
  then: Expr;
  else?: Expr;
};

export type LoopId = number;

export type ExprLoop = {
  kind: "loop";
  body: Expr;
  loopId: LoopId;
};

export type ExprBreak = {
  kind: "break";
  target?: LoopId;
};

export type ExprStructLiteral = {
  kind: "structLiteral";
  name: Identifier;
  fields: [Identifier, Expr][];
};

export type TupleLiteral = {
  kind: "tupleLiteral";
  fields: Expr[];
};

export type ExprKind =
  | ExprEmpty
  | ExprLet
  | ExprAssign
  | ExprBlock
  | ExprLiteral
  | ExprIdent
  | ExprPath
  | ExprBinary
  | ExprUnary
  | ExprCall
  | ExprFieldAccess
  | ExprIf
  | ExprLoop
  | ExprBreak
  | ExprStructLiteral
  | TupleLiteral;

export type Expr = ExprKind & {
  span: Span;
  ty?: Ty;
};

export type StringLiteral = {
  kind: "str";
  value: string;
  span: Span;
};

export type Literal =
  | StringLiteral
  | {
      kind: "int";
      value: number;
      type: LitIntType;
    };

export type BinaryKind =
  | "+"
  | "-"
  | "*"
  | "/"
  | "%"
  | "&"
  | "|"
  | "<"
  | ">"
  | "=="
  | "<="
  | ">="
  | "!=";

export const COMPARISON_KINDS: BinaryKind[] = [
  ">",
  "<",
  "==",
  "<=",
  ">=",
  "!=",
];
export const EQUALITY_KINDS: BinaryKind[] = ["==", "!="];
export const LOGICAL_KINDS: BinaryKind[] = ["&", "|"];
export const ARITH_TERM_KINDS: BinaryKind[] = ["+", "-"];
export const ARITH_FACTOR_KINDS: BinaryKind[] = ["*", "/", "%"];

const BINARY_KIND_PREC_CLASS = new Map<BinaryKind, number>([
  ["+", 0],
  ["-", 0],
  ["*", 0],
  ["/", 0],
  ["%", 0],
  ["&", 1],
  ["|", 2],
  ["<", 3],
  [">", 4],
  ["==", 5],
  ["<=", 6],
  [">=", 7],
  ["!=", 8],
]);

export function binaryExprPrecedenceClass(k: BinaryKind): number {
  const cls = BINARY_KIND_PREC_CLASS.get(k);
  if (cls === undefined) {
    throw new Error(`Invalid binary kind: '${k}'`);
  }
  return cls;
}

export type UnaryKind = "!" | "-";
export const UNARY_KINDS: UnaryKind[] = ["!", "-"];

export type TypeKind =
  | {
      kind: "ident";
      value: Identifier;
    }
  | {
      kind: "list";
      elem: Type;
    }
  | {
      kind: "tuple";
      elems: Type[];
    }
  | { kind: "never" };

export type Type = TypeKind & {
  span: Span;
  ty?: Ty;
};

// name resolution stuff

export type Resolution =
  | {
      kind: "local";
      /**
       * The index of the local variable, from inside out.
       * ```
       * let a; let b; (a, b);
       *     ^      ^
       *     1      0
       * ```
       * When traversing resolutions, a stack of locals has to be kept.
       * It's similar to a De Bruijn index.
       */
      index: number;
    }
  | {
      kind: "item";
      id: ItemId;
    }
  | {
      kind: "builtin";
      name: BuiltinName;
    };

export const BUILTINS = [
  "print",
  "String",
  "Int",
  "I32",
  "Bool",
  "true",
  "false",
  "trap",
  // Intrinsics:
  "__i32_store",
  "__i64_store",
  "__i32_load",
  "__i64_load",
  "__string_ptr",
  "__string_len",
] as const;

export type BuiltinName = (typeof BUILTINS)[number];

export type LocalInfo = {
  name: string;
  span: Span;
  ty?: Ty;
};

// types

export type TyString = {
  kind: "string";
};

export type TyInt = {
  kind: "int";
};

export type TyI32 = {
  kind: "i32";
};

export type TyBool = {
  kind: "bool";
};

export type TyList = {
  kind: "list";
  elem: Ty;
};

export type TyTuple = {
  kind: "tuple";
  elems: Ty[];
};

export type TyUnit = {
  kind: "tuple";
  elems: [];
};

export type TyFn = {
  kind: "fn";
  params: Ty[];
  returnTy: Ty;
};

export type TyVar = {
  kind: "var";
  index: number;
};

export type TyStruct = {
  kind: "struct";
  name: string;
  fields: [string, Ty][];
};

export type TyNever = {
  kind: "never";
};

export type Ty =
  | TyString
  | TyInt
  | TyI32
  | TyBool
  | TyList
  | TyTuple
  | TyFn
  | TyVar
  | TyStruct
  | TyNever;

export function tyIsUnit(ty: Ty): ty is TyUnit {
  return ty.kind === "tuple" && ty.elems.length === 0;
}

export const TY_UNIT: Ty = { kind: "tuple", elems: [] };
export const TY_STRING: Ty = { kind: "string" };
export const TY_BOOL: Ty = { kind: "bool" };
export const TY_INT: Ty = { kind: "int" };
export const TY_I32: Ty = { kind: "i32" };
export const TY_NEVER: Ty = { kind: "never" };

export type TypeckResults = {
  main: Resolution;
};

// folders

export type FoldFn<T> = (value: T) => T;

export type Folder = {
  ast: () => Ast;
  /**
   * This should not be overridden.
   */
  item: FoldFn<Item>;
  itemInner: FoldFn<Item>;
  expr: FoldFn<Expr>;
  ident: FoldFn<Identifier>;
  type: FoldFn<Type>;
};

const ITEM_DEFAULT = Symbol("item must not be overriden");

export const DEFAULT_FOLDER: Folder = {
  ast() {
    throw new Error("folders need to implement `ast`");
  },
  item(item) {
    const newItem = this.itemInner(item);    
    this.ast().itemsById.set(item.id, newItem);
    return newItem;
  },
  itemInner(item) {
    return superFoldItem(item, this);
  },
  expr(expr) {
    return superFoldExpr(expr, this);
  },
  ident(ident) {
    return ident;
  },
  type(type) {
    return superFoldType(type, this);
  },
};
(DEFAULT_FOLDER.item as any)[ITEM_DEFAULT] = ITEM_DEFAULT;

export function foldAst(ast: Ast, folder: Folder): Ast {
  if ((folder.item as any)[ITEM_DEFAULT] !== ITEM_DEFAULT) {
    throw new Error("must not override `item` on folders");
  }

  return {
    rootItems: ast.rootItems.map((item) => folder.item(item)),
    itemsById: ast.itemsById,
    typeckResults: ast.typeckResults,
    packageName: ast.packageName,
  };
}

export function superFoldItem(item: Item, folder: Folder): Item {
  switch (item.kind) {
    case "function": {
      const args = item.node.params.map(({ name, type, span }) => ({
        name,
        type: folder.type(type),
        span,
      }));

      return {
        ...item,
        kind: "function",
        node: {
          name: item.node.name,
          params: args,
          body: folder.expr(item.node.body),
          returnType: item.node.returnType && folder.type(item.node.returnType),
        },
      };
    }
    case "type": {
      const fields = item.node.fields.map(({ name, type }) => ({
        name,
        type: folder.type(type),
      }));

      return {
        ...item,
        kind: "type",
        node: { name: item.node.name, fields },
      };
    }
    case "import": {
      const args = item.node.params.map(({ name, type, span }) => ({
        name,
        type: folder.type(type),
        span,
      }));
      return {
        ...item,
        kind: "import",
        node: {
          module: item.node.module,
          func: item.node.func,
          name: item.node.name,
          params: args,
          returnType: item.node.returnType && folder.type(item.node.returnType),
        },
      };
    }
    case "mod": {
      let kind: ModItemKind;
      const { modKind: itemKind } = item.node;
      switch (itemKind.kind) {
        case "inline":
          kind = {
            kind: "inline",
            contents: itemKind.contents.map((item) => folder.item(item)),
          };
          break;
        case "extern":
          kind = { kind: "extern" };
          break;
      }

      return {
        ...item,
        kind: "mod",
        node: {
          name: item.node.name,
          modKind: kind,
        },
      };
    }
  }
}

export function superFoldExpr(expr: Expr, folder: Folder): Expr {
  const span = expr.span;
  switch (expr.kind) {
    case "empty": {
      return { kind: "empty", span };
    }
    case "let": {
      return {
        ...expr,
        kind: "let",
        name: expr.name,
        type: expr.type && folder.type(expr.type),
        rhs: folder.expr(expr.rhs),
      };
    }
    case "assign": {
      return {
        ...expr,
        kind: "assign",
        lhs: folder.expr(expr.lhs),
        rhs: folder.expr(expr.rhs),
      };
    }
    case "block": {
      return {
        ...expr,
        kind: "block",
        exprs: expr.exprs.map((expr) => folder.expr(expr)),
      };
    }
    case "literal": {
      return { kind: "literal", value: expr.value, span };
    }
    case "ident": {
      return { kind: "ident", value: folder.ident(expr.value), span };
    }
    case "path": {
      return { ...expr, kind: "path" };
    }
    case "binary": {
      return {
        ...expr,
        kind: "binary",
        binaryKind: expr.binaryKind,
        lhs: folder.expr(expr.lhs),
        rhs: folder.expr(expr.rhs),
      };
    }
    case "unary": {
      return {
        ...expr,
        kind: "unary",
        unaryKind: expr.unaryKind,
        rhs: folder.expr(expr.rhs),
      };
    }
    case "call": {
      return {
        ...expr,
        kind: "call",
        lhs: folder.expr(expr.lhs),
        args: expr.args.map((expr) => folder.expr(expr)),
      };
    }
    case "fieldAccess": {
      return {
        ...expr,
        kind: "fieldAccess",
        lhs: folder.expr(expr.lhs),
      };
    }
    case "if": {
      return {
        ...expr,
        kind: "if",
        cond: folder.expr(expr.cond),
        then: folder.expr(expr.then),
        else: expr.else && folder.expr(expr.else),
      };
    }
    case "loop": {
      return {
        ...expr,
        kind: "loop",
        body: folder.expr(expr.body),
      };
    }
    case "break": {
      return {
        ...expr,
        kind: "break",
      };
    }
    case "structLiteral": {
      return {
        ...expr,
        kind: "structLiteral",
        name: folder.ident(expr.name),
        fields: expr.fields.map(([name, expr]) => [name, folder.expr(expr)]),
      };
    }
    case "tupleLiteral": {
      return {
        ...expr,
        kind: "tupleLiteral",
        fields: expr.fields.map(folder.expr.bind(folder)),
      };
    }
  }
}

export function superFoldType(type: Type, folder: Folder): Type {
  const span = type.span;
  switch (type.kind) {
    case "ident": {
      return {
        kind: "ident",
        value: folder.ident(type.value),
        span,
      };
    }
    case "list": {
      return {
        kind: "list",
        elem: folder.type(type.elem),
        span,
      };
    }
    case "tuple": {
      return {
        kind: "tuple",
        elems: type.elems.map((type) => folder.type(type)),
        span,
      };
    }
    case "never": {
      return { ...type, kind: "never" };
    }
  }
}

export function varUnreachable(): never {
  throw new Error("Type variables must not occur after type checking");
}

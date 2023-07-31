import { Span } from "./error";
import { LitIntType } from "./lexer";

export type Phase = {
  res: unknown;
  defPath: unknown;
  ty: unknown;
  typeckResults: unknown;
};

type No = object;

type HasRes = { res: Resolution };
type HasDefPath = { defPath: string[] };
type HasTy = { ty: Ty };
type HasTypeckResults = { typeckResults: TypeckResults };

export type Parsed = {
  res: No;
  defPath: No;
  ty: No;
  typeckResults: No;
};
export type Built = {
  res: No;
  defPath: No;
  ty: No;
  typeckResults: No;
};
export type Resolved = {
  res: HasRes;
  defPath: HasDefPath;
  ty: No;
  typeckResults: No;
};
export type Typecked = {
  res: HasRes;
  defPath: HasDefPath;
  ty: HasTy;
  typeckResults: HasTypeckResults;
};

export type AnyPhase = {
  res: No | HasRes;
  defPath: No | HasDefPath;
  ty: No | HasTy;
  typeckResults: No | HasTypeckResults;
};

export type CrateId = number;

export type Crate<P extends Phase> = {
  id: CrateId;
  rootItems: Item<P>[];
  itemsById: Map<ItemId, Item<P>>;
  packageName: string;
} & P["typeckResults"];

export type Ident = {
  name: string;
  span: Span;
};

export type IdentWithRes<P extends Phase> = {
  name: string;
  span: Span;
} & P["res"];

export type ItemId = number;

export type ItemKind<P extends Phase> =
  | {
      kind: "function";
      node: FunctionDef<P>;
    }
  | {
      kind: "type";
      node: TypeDef<P>;
    }
  | {
      kind: "import";
      node: ImportDef<P>;
    }
  | {
      kind: "mod";
      node: ModItem<P>;
    }
  | {
      kind: "extern";
      node: ExternItem;
    };

export type Item<P extends Phase> = ItemKind<P> & {
  span: Span;
  id: ItemId;
} & P["defPath"];

export type FunctionDef<P extends Phase> = {
  name: string;
  params: FunctionArg<P>[];
  body: Expr<P>;
  returnType?: Type<P>;
  ty?: TyFn;
};

export type FunctionArg<P extends Phase> = {
  name: string;
  type: Type<P>;
  span: Span;
};

export type TypeDef<P extends Phase> = {
  name: string;
  fields: FieldDef<P>[];
  ty?: TyStruct;
};

export type FieldDef<P extends Phase> = {
  name: Ident;
  type: Type<P>;
};

export type ImportDef<P extends Phase> = {
  module: StringLiteral;
  func: StringLiteral;
  name: string;
  params: FunctionArg<P>[];
  returnType?: Type<P>;
  ty?: TyFn;
};

export type ModItem<P extends Phase> = {
  name: string;
  contents: Item<P>[];
};

export type ExternItem = { name: string };

export type ExprEmpty = { kind: "empty" };

export type ExprLet<P extends Phase> = {
  kind: "let";
  name: Ident;
  type?: Type<P>;
  rhs: Expr<P>;
  // IMPORTANT: This is (sadly) shared with ExprBlock.
  // TODO: Stop this sharing and just store the stack of blocks in typeck.
  local?: LocalInfo;
};

// A bit like ExprBinary except there are restrictions
// on the LHS and precedence is unrestricted.
export type ExprAssign<P extends Phase> = {
  kind: "assign";
  lhs: Expr<P>;
  rhs: Expr<P>;
};

export type ExprBlock<P extends Phase> = {
  kind: "block";
  exprs: Expr<P>[];
  // IMPORTANT: This is (sadly) shared with ExprLet.
  locals?: LocalInfo[];
};

export type ExprLiteral = {
  kind: "literal";
  value: Literal;
};

export type ExprIdent<P extends Phase> = {
  kind: "ident";
  value: IdentWithRes<P>;
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

export type ExprBinary<P extends Phase> = {
  kind: "binary";
  binaryKind: BinaryKind;
  lhs: Expr<P>;
  rhs: Expr<P>;
};

export type ExprUnary<P extends Phase> = {
  kind: "unary";
  unaryKind: UnaryKind;
  rhs: Expr<P>;
};

export type ExprCall<P extends Phase> = {
  kind: "call";
  lhs: Expr<P>;
  args: Expr<P>[];
};

export type ExprFieldAccess<P extends Phase> = {
  kind: "fieldAccess";
  lhs: Expr<P>;
  field: {
    value: string | number;
    span: Span;
    fieldIdx?: number;
  };
};

export type ExprIf<P extends Phase> = {
  kind: "if";
  cond: Expr<P>;
  then: Expr<P>;
  else?: Expr<P>;
};

export type LoopId = number;

export type ExprLoop<P extends Phase> = {
  kind: "loop";
  body: Expr<P>;
  loopId: LoopId;
};

export type ExprBreak = {
  kind: "break";
  target?: LoopId;
};

export type ExprStructLiteral<P extends Phase> = {
  kind: "structLiteral";
  name: IdentWithRes<P>;
  fields: [Ident, Expr<P>][];
};

export type TupleLiteral<P extends Phase> = {
  kind: "tupleLiteral";
  fields: Expr<P>[];
};

export type ExprKind<P extends Phase> =
  | ExprEmpty
  | ExprLet<P>
  | ExprAssign<P>
  | ExprBlock<P>
  | ExprLiteral
  | ExprIdent<P>
  | ExprPath
  | ExprBinary<P>
  | ExprUnary<P>
  | ExprCall<P>
  | ExprFieldAccess<P>
  | ExprIf<P>
  | ExprLoop<P>
  | ExprBreak
  | ExprStructLiteral<P>
  | TupleLiteral<P>;

export type Expr<P extends Phase> = ExprKind<P> & {
  span: Span;
} & P["ty"];

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

export type TypeKind<P extends Phase> =
  | {
      kind: "ident";
      value: IdentWithRes<P>;
    }
  | {
      kind: "list";
      elem: Type<P>;
    }
  | {
      kind: "tuple";
      elems: Type<P>[];
    }
  | { kind: "never" };

export type Type<P extends Phase> = TypeKind<P> & {
  span: Span;
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
  main: Resolution | undefined;
};

// folders

export type FoldFn<From, To> = (value: From) => To;

export type Folder<From extends Phase, To extends Phase> = {
  newItemsById: Map<ItemId, Item<To>>;
  /**
   * This should not be overridden.
   */
  item: FoldFn<Item<From>, Item<To>>;
  itemInner: FoldFn<Item<From>, Item<To>>;
  expr: FoldFn<Expr<From>, Expr<To>>;
  ident: FoldFn<IdentWithRes<From>, IdentWithRes<To>>;
  type: FoldFn<Type<From>, Type<To>>;
};

type ItemFolder<From extends Phase, To extends Phase> = {
  newItemsById: Map<ItemId, Item<To>>;
  item: FoldFn<Item<From>, Item<To>>;
  itemInner: FoldFn<Item<From>, Item<To>>;
};

const ITEM_DEFAULT = Symbol("item must not be overriden");

export function mkDefaultFolder<
  From extends Phase,
  To extends Phase
>(): ItemFolder<From, To> {
  const folder: ItemFolder<From, To> = {
    newItemsById: new Map(),
    item(item) {
      const newItem = this.itemInner(item);
      this.newItemsById.set(newItem.id, newItem);
      return newItem;
    },
    itemInner(_item) {
      throw new Error("unimplemented");
    },
  };
  (folder.item as any)[ITEM_DEFAULT] = ITEM_DEFAULT;

  return folder;
}

export function foldAst<From extends Phase, To extends Phase>(
  ast: Crate<From>,
  folder: Folder<From, To>
): Crate<To> {
  if ((folder.item as any)[ITEM_DEFAULT] !== ITEM_DEFAULT) {
    throw new Error("must not override `item` on folders");
  }

  return {
    id: ast.id,
    rootItems: ast.rootItems.map((item) => folder.item(item)),
    itemsById: folder.newItemsById,
    typeckResults: "typeckResults" in ast ? ast.typeckResults : undefined,
    packageName: ast.packageName,
  };
}

export function superFoldItem<From extends Phase, To extends Phase>(
  item: Item<From>,
  folder: Folder<From, To>
): Item<To> {
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
      return {
        ...item,
        kind: "mod",
        node: {
          name: item.node.name,
          contents: item.node.contents.map((item) => folder.item(item)),
        },
      };
    }
    case "extern": {
      return { ...item, kind: "extern" };
    }
  }
}

export function superFoldExpr<From extends Phase, To extends Phase>(
  expr: Expr<From>,
  folder: Folder<From, To>
): Expr<To> {
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

export function superFoldType<From extends Phase, To extends Phase>(
  type: Type<From>,
  folder: Folder<From, To>
): Type<To> {
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

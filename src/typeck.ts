import {
  Ast,
  binaryExprPrecedenceClass,
  COMPARISON_KINDS,
  DEFAULT_FOLDER,
  EQUALITY_KINDS,
  Expr,
  ExprBinary,
  ExprUnary,
  foldAst,
  Folder,
  Identifier,
  ItemId,
  LOGICAL_KINDS,
  Resolution,
  Ty,
  TY_BOOL,
  TY_INT,
  TY_STRING,
  TY_UNIT,
  TyFn,
  Type,
  TyStruct,
} from "./ast";
import { CompilerError, Span } from "./error";
import { printTy } from "./printer";

function builtinAsTy(name: string, span: Span): Ty {
  switch (name) {
    case "String": {
      return TY_STRING;
    }
    case "Int": {
      return TY_INT;
    }
    case "Bool": {
      return TY_BOOL;
    }
    default: {
      throw new CompilerError(`\`${name}\` is not a type`, span);
    }
  }
}

function typeOfBuiltinValue(name: string, span: Span): Ty {
  switch (name) {
    case "false":
    case "true":
      return TY_BOOL;
    case "print":
      return { kind: "fn", params: [TY_STRING], returnTy: TY_UNIT };
    default: {
      throw new CompilerError(`\`${name}\` cannot be used as a value`, span);
    }
  }
}

function lowerAstTyBase(
  type: Type,
  lowerIdentTy: (ident: Identifier) => Ty,
  typeOfItem: (index: number) => Ty
): Ty {
  switch (type.kind) {
    case "ident": {
      return lowerIdentTy(type.value);
    }
    case "list": {
      return {
        kind: "list",
        elem: lowerAstTyBase(type.elem, lowerIdentTy, typeOfItem),
      };
    }
    case "tuple": {
      return {
        kind: "tuple",
        elems: type.elems.map((type) =>
          lowerAstTyBase(type, lowerIdentTy, typeOfItem)
        ),
      };
    }
  }
}

export function typeck(ast: Ast): Ast {
  const itemTys = new Map<number, Ty | null>();
  function typeOfItem(index: ItemId): Ty {
    const item = ast.items[index];

    const ty = itemTys.get(index);
    if (ty) {
      return ty;
    }
    if (ty === null) {
      throw new CompilerError(`cycle computing type of #G${index}`, item.span);
    }
    itemTys.set(index, null);
    switch (item.kind) {
      case "function": {
        const args = item.node.params.map((arg) => lowerAstTy(arg.type));
        const returnTy: Ty = item.node.returnType
          ? lowerAstTy(item.node.returnType)
          : TY_UNIT;

        const ty: Ty = { kind: "fn", params: args, returnTy };
        itemTys.set(item.id, ty);
        return ty;
      }
      case "type": {
        const ty: Ty = {
          kind: "struct",
          name: item.node.name,
          fields: [
            /*dummy*/
          ],
        };

        itemTys.set(item.id, ty);

        const fields = item.node.fields.map<[string, Ty]>(({ name, type }) => [
          name.name,
          lowerAstTy(type),
        ]);

        ty.fields = fields;
        return ty;
      }
    }
  }

  function lowerAstTy(type: Type): Ty {
    return lowerAstTyBase(
      type,
      (ident) => {
        const res = ident.res!;
        switch (res.kind) {
          case "local": {
            throw new Error("Item type cannot refer to local variable");
          }
          case "item": {
            return typeOfItem(res.index);
          }
          case "builtin": {
            return builtinAsTy(res.name, ident.span);
          }
        }
      },
      typeOfItem
    );
  }

  const checker: Folder = {
    ...DEFAULT_FOLDER,
    item(item) {
      switch (item.kind) {
        case "function": {
          const fnTy = typeOfItem(item.id) as TyFn;
          const body = checkBody(item.node.body, fnTy, typeOfItem);

          const returnType = item.node.returnType && {
            ...item.node.returnType,
            ty: fnTy.returnTy,
          };
          return {
            ...item,
            node: {
              name: item.node.name,
              params: item.node.params.map((arg, i) => ({
                ...arg,
                type: { ...arg.type, ty: fnTy.params[i] },
              })),
              body,
              returnType,
              ty: fnTy,
            },
          };
        }
        case "type": {
          const fieldNames = new Set();
          item.node.fields.forEach(({ name }) => {
            if (fieldNames.has(name)) {
              throw new CompilerError(
                `type ${item.node.name} has a duplicate field: ${name.name}`,
                name.span
              );
            }
            fieldNames.add(name);
          });

          const ty = typeOfItem(item.id) as TyStruct;

          return {
            ...item,
            node: {
              name: item.node.name,
              fields: item.node.fields.map((field, i) => ({
                name: field.name,
                type: {
                  ...field.type,
                  ty: ty.fields[i][1],
                },
              })),
            },
          };
        }
      }
    },
  };

  const typecked = foldAst(ast, checker);

  const main = typecked.items.find((item) => {
    if (item.kind === "function" && item.node.name === "main") {
      const func = item.node;
      if (func.returnType !== undefined) {
        const ty = func.returnType.ty!;
        if (ty.kind !== "tuple" || ty.elems.length !== 0) {
          throw new CompilerError(
            `\`main\` has an invalid signature. main takes no arguments and returns nothing`,
            item.span
          );
        }
      }

      return true;
    }
    return false;
  });

  if (!main) {
    throw new CompilerError(`\`main\` function not found`, {
      start: 0,
      end: 1,
    });
  }

  typecked.typeckResults = {
    main: main.id,
  };

  return typecked;
}

type TyVarRes =
  | {
      kind: "final";
      ty: Ty;
    }
  | {
      kind: "unified";
      index: number;
    }
  | {
      kind: "unknown";
    };

export class InferContext {
  tyVars: TyVarRes[] = [];

  public newVar(): Ty {
    const index = this.tyVars.length;
    this.tyVars.push({ kind: "unknown" });
    return { kind: "var", index };
  }

  private tryResolveVar(variable: number): Ty | undefined {
    const varRes = this.tyVars[variable];
    switch (varRes.kind) {
      case "final": {
        return varRes.ty;
      }
      case "unified": {
        const ty = this.tryResolveVar(varRes.index);
        if (ty) {
          this.tyVars[variable] = { kind: "final", ty };
          return ty;
        } else {
          return undefined;
        }
      }
      case "unknown": {
        return undefined;
      }
    }
  }

  private findRoot(variable: number): number {
    let root = variable;
    let nextVar;
    while ((nextVar = this.tyVars[root]).kind === "unified") {
      root = nextVar.index;
    }
    return root;
  }

  /**
   * Try to constrain a type variable to be of a specific type.
   * INVARIANT: Both sides must not be of res "final", use resolveIfPossible
   * before calling this.
   */
  private constrainVar(variable: number, ty: Ty) {
    let root = this.findRoot(variable);

    if (ty.kind === "var") {
      // Now we point our root to the other root to unify the two graphs.
      const otherRoot = this.findRoot(ty.index);

      // If both types have the same root, we don't need to do anything
      // as they're already part of the same graph.
      if (root != otherRoot) {
        this.tyVars[root] = { kind: "unified", index: otherRoot };
      }
    } else {
      this.tyVars[root] = { kind: "final", ty };
    }
  }

  public resolveIfPossible(ty: Ty): Ty {
    // TODO: dont be shallow resolve
    // note that fixing this will cause cycles. fix those cycles instead using
    // he fancy occurs check as errs called it.
    if (ty.kind === "var") {
      return this.tryResolveVar(ty.index) ?? ty;
    } else {
      return ty;
    }
  }

  public assign(lhs_: Ty, rhs_: Ty, span: Span) {
    const lhs = this.resolveIfPossible(lhs_);
    const rhs = this.resolveIfPossible(rhs_);

    if (lhs.kind === "var") {
      this.constrainVar(lhs.index, rhs);
      return;
    }
    if (rhs.kind === "var") {
      this.constrainVar(rhs.index, lhs);
      return;
    }
    // type variable handling here

    switch (lhs.kind) {
      case "string": {
        if (rhs.kind === "string") return;
        break;
      }
      case "int": {
        if (rhs.kind === "int") return;
        break;
      }
      case "bool": {
        if (rhs.kind === "bool") return;
        break;
      }
      case "list": {
        if (rhs.kind === "list") {
          this.assign(lhs.elem, rhs.elem, span);
          return;
        }
        break;
      }
      case "tuple": {
        if (rhs.kind === "tuple" && lhs.elems.length === rhs.elems.length) {
          lhs.elems.forEach((lhs, i) => this.assign(lhs, rhs.elems[i], span));
          return;
        }
        break;
      }
      case "fn": {
        if (rhs.kind === "fn" && lhs.params.length === rhs.params.length) {
          // swapping because of contravariance in the future maybe
          lhs.params.forEach((lhs, i) => this.assign(rhs.params[i], lhs, span));

          this.assign(lhs.returnTy, rhs.returnTy, span);

          return;
        }
        break;
      }
      case "struct": {
        if (rhs.kind === "struct" && lhs.name === rhs.name) {
          return;
        }
      }
    }

    throw new CompilerError(
      `cannot assign ${printTy(rhs)} to ${printTy(lhs)}`,
      span
    );
  }
}

export function checkBody(
  body: Expr,
  fnTy: TyFn,
  typeOfItem: (index: number) => Ty
): Expr {
  const localTys = [...fnTy.params];

  const infcx = new InferContext();

  function typeOf(res: Resolution, span: Span): Ty {
    switch (res.kind) {
      case "local": {
        const idx = localTys.length - 1 - res.index;
        return localTys[idx];
      }
      case "item": {
        return typeOfItem(res.index);
      }
      case "builtin":
        return typeOfBuiltinValue(res.name, span);
    }
  }

  function lowerAstTy(type: Type): Ty {
    return lowerAstTyBase(
      type,
      (ident) => {
        const res = ident.res!;
        return typeOf(res, ident.span);
      },
      typeOfItem
    );
  }

  const checker: Folder = {
    ...DEFAULT_FOLDER,
    expr(expr) {
      switch (expr.kind) {
        case "empty": {
          return { ...expr, ty: TY_UNIT };
        }
        case "let": {
          const loweredBindingTy = expr.type && lowerAstTy(expr.type);
          let bindingTy = loweredBindingTy ? loweredBindingTy : infcx.newVar();

          const rhs = this.expr(expr.rhs);
          infcx.assign(bindingTy, rhs.ty!, expr.span);

          // AST validation ensures that lets can only be in blocks, where
          // the types will be popped.
          localTys.push(bindingTy);

          expr.local!.ty = bindingTy;

          const type: Type | undefined = loweredBindingTy && {
            ...expr.type!,
            ty: loweredBindingTy!,
          };

          return {
            kind: "let",
            name: expr.name,
            type,
            rhs,
            ty: TY_UNIT,
            span: expr.span,
          };
        }
        case "block": {
          const prevLocalTysLen = localTys.length;

          const exprs = expr.exprs.map((expr) => this.expr(expr));

          const ty = exprs.length > 0 ? exprs[exprs.length - 1].ty! : TY_UNIT;

          localTys.length = prevLocalTysLen;

          return {
            ...expr,
            exprs,
            ty,
          };
        }
        case "literal": {
          let ty;
          switch (expr.value.kind) {
            case "str": {
              ty = TY_STRING;
              break;
            }
            case "int": {
              ty = TY_INT;
              break;
            }
          }

          return { ...expr, ty };
        }
        case "ident": {
          const ty = typeOf(expr.value.res!, expr.value.span);

          return { ...expr, ty };
        }
        case "binary": {
          const lhs = this.expr(expr.lhs);
          const rhs = this.expr(expr.rhs);

          lhs.ty = infcx.resolveIfPossible(lhs.ty!);
          rhs.ty = infcx.resolveIfPossible(rhs.ty!);

          return checkBinary({ ...expr, lhs, rhs });
        }
        case "unary": {
          const rhs = this.expr(expr.rhs);
          rhs.ty = infcx.resolveIfPossible(rhs.ty!);
          return checkUnary({ ...expr, rhs });
        }
        case "call": {
          const lhs = this.expr(expr.lhs);
          lhs.ty = infcx.resolveIfPossible(lhs.ty!);
          const lhsTy = lhs.ty!;
          if (lhsTy.kind !== "fn") {
            throw new CompilerError(
              `expression of type ${printTy(lhsTy)} is not callable`,
              lhs.span
            );
          }

          const args = expr.args.map((arg) => this.expr(arg));

          lhsTy.params.forEach((param, i) => {
            if (!args[i]) {
              throw new CompilerError(
                `missing argument of type ${printTy(param)}`,
                expr.span
              );
            }
            const arg = checker.expr(args[i]);

            infcx.assign(param, arg.ty!, args[i].span);
          });

          if (args.length > lhsTy.params.length) {
            throw new CompilerError(
              `too many arguments passed, expected ${lhsTy.params.length}, found ${args.length}`,
              expr.span
            );
          }

          return { ...expr, lhs, args, ty: lhsTy.returnTy };
        }
        case "if": {
          const cond = this.expr(expr.cond);
          const then = this.expr(expr.then);
          const elsePart = expr.else && this.expr(expr.else);

          infcx.assign(TY_BOOL, cond.ty!, cond.span);

          let ty: Ty;
          if (elsePart) {
            infcx.assign(then.ty!, elsePart.ty!, elsePart.span);
            ty = then.ty!;
          } else {
            infcx.assign(TY_UNIT, then.ty!, then.span);
            ty = TY_UNIT;
          }

          return { ...expr, cond, then, else: elsePart, ty };
        }
        case "structLiteral": {
          const fields = expr.fields.map<[Identifier, Expr]>(([name, expr]) => [
            name,
            this.expr(expr),
          ]);

          const structTy = typeOf(expr.name.res!, expr.name.span);

          if (structTy.kind !== "struct") {
            throw new CompilerError(
              `struct literal is only allowed for struct types`,
              expr.span
            );
          }

          const assignedFields = new Set();

          fields.forEach(([name, field], i) => {
            const fieldTy = structTy.fields.find((def) => def[0] === name.name);
            if (!fieldTy) {
              throw new CompilerError(
                `field ${name.name} doesn't exist on type ${expr.name.name}`,
                name.span
              );
            }
            infcx.assign(fieldTy[1], field.ty!, field.span);
            assignedFields.add(name.name);
          });

          const missing: string[] = [];
          structTy.fields.forEach(([name]) => {
            if (!assignedFields.has(name)) {
              missing.push(name);
            }
          });
          if (missing.length > 0) {
            throw new CompilerError(
              `missing fields in literal: ${missing.join(", ")}`,
              expr.span
            );
          }

          return { ...expr, fields, ty: structTy };
        }
      }
    },
  };

  const checked = checker.expr(body);

  infcx.assign(fnTy.returnTy, checked.ty!, body.span);

  const resolveTy = (ty: Ty, span: Span) => {
    const resTy = infcx.resolveIfPossible(ty);
    if (!resTy) {
      throw new CompilerError("cannot infer type", span);
    }
    return resTy;
  };

  const resolver: Folder = {
    ...DEFAULT_FOLDER,
    expr(expr) {
      const ty = resolveTy(expr.ty!, expr.span);

      if (expr.kind === "block") {
        expr.locals!.forEach((local) => {
          local.ty = resolveTy(local.ty!, local.span);
        });
      }

      return { ...expr, ty };
    },
  };

  const resolved = resolver.expr(checked);

  return resolved;
}

function checkBinary(expr: Expr & ExprBinary): Expr {
  const checkPrecedence = (inner: Expr, side: string) => {
    if (inner.kind === "binary") {
      const ourClass = binaryExprPrecedenceClass(expr.binaryKind);
      const innerClass = binaryExprPrecedenceClass(inner.binaryKind);

      if (ourClass !== innerClass) {
        throw new CompilerError(
          `mixing operators without parentheses is not allowed. ${side} is ${inner.binaryKind}, which is different from ${expr.binaryKind}`,
          expr.span
        );
      }
    }
  };

  checkPrecedence(expr.lhs, "left");
  checkPrecedence(expr.rhs, "right");

  let lhsTy = expr.lhs.ty!;
  let rhsTy = expr.rhs.ty!;

  if (COMPARISON_KINDS.includes(expr.binaryKind)) {
    if (lhsTy.kind === "int" && rhsTy.kind === "int") {
      return { ...expr, ty: TY_BOOL };
    }

    if (lhsTy.kind === "string" && rhsTy.kind === "string") {
      return { ...expr, ty: TY_BOOL };
    }

    if (EQUALITY_KINDS.includes(expr.binaryKind)) {
      if (lhsTy.kind === "bool" && rhsTy.kind === "bool") {
        return { ...expr, ty: TY_BOOL };
      }
    }
  }

  if (lhsTy.kind === "int" && rhsTy.kind === "int") {
    return { ...expr, ty: TY_INT };
  }

  if (LOGICAL_KINDS.includes(expr.binaryKind)) {
    if (lhsTy.kind === "bool" && rhsTy.kind === "bool") {
      return { ...expr, ty: TY_BOOL };
    }
  }

  throw new CompilerError(
    `invalid types for binary operation: ${printTy(expr.lhs.ty!)} ${
      expr.binaryKind
    } ${printTy(expr.rhs.ty!)}`,
    expr.span
  );
}

function checkUnary(expr: Expr & ExprUnary): Expr {
  let rhsTy = expr.rhs.ty!;

  if (
    expr.unaryKind === "!" &&
    (rhsTy.kind === "int" || rhsTy.kind === "bool")
  ) {
    return { ...expr, ty: rhsTy };
  }

  if (expr.unaryKind === "-" && rhsTy.kind == "int") {
    // Negating an unsigned integer is a bad idea.
  }

  throw new CompilerError(
    `invalid types for unary operation: ${expr.unaryKind} ${printTy(
      expr.rhs.ty!
    )}`,
    expr.span
  );
}

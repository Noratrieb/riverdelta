import { CompilerError, ErrorHandler, Span } from "../error";
import { printTy } from "../printer";
import { Ty } from "../types";

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

  constructor(public error: ErrorHandler) {}

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
    const root = this.findRoot(variable);

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
    // the fancy occurs check as errs called it.
    if (ty.kind === "var") {
      return this.tryResolveVar(ty.index) ?? ty;
    } else {
      return ty;
    }
  }

  public assign(lhs_: Ty, rhs_: Ty, span: Span): void {
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

    if (lhs.kind === "error" || rhs.kind === "error") {
      // This Is Fine 🐶🔥.
      return;
    }

    if (rhs.kind === "never") {
      // not sure whether this is entirely correct wrt inference.. it will work out, probably.
      return;
    }

    switch (lhs.kind) {
      case "string": {
        if (rhs.kind === "string") return;
        break;
      }
      case "int": {
        if (rhs.kind === "int") return;
        break;
      }
      case "i32": {
        if (rhs.kind === "i32") return;
        break;
      }
      case "bool": {
        if (rhs.kind === "bool") return;
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
        if (rhs.kind === "struct" && lhs.itemId === rhs.itemId) {
          return;
        }
        break;
      }
      case "rawptr": {
        if (rhs.kind === "rawptr") {
          this.assign(lhs.inner, rhs.inner, span);
          return;
        }
        break;
      }
    }

    this.error.emitError(
      new CompilerError(
        `cannot assign ${printTy(rhs)} to ${printTy(lhs)}`,
        span,
      ),
    );
  }
}

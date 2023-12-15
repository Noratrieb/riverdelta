import { ItemId, Pkg, Resolved, Ty } from "../ast";
import { GlobalContext } from "../context";
import { CompilerError, ErrorEmitted } from "../error";
import { ComplexMap } from "../utils";

export type TypeckCtx = {
  gcx: GlobalContext;
  /**
   * A cache of all item types.
   * Starts off as undefined, then gets set to null
   * while computing the type (for cycle detection) and
   * afterwards, we get the ty.
   */
  itemTys: ComplexMap<ItemId, Ty | null>;
  ast: Pkg<Resolved>;
};

export function mkTyFn(params: Ty[], returnTy: Ty): Ty {
  return { kind: "fn", params, returnTy };
}

export function tyError(cx: TypeckCtx, err: CompilerError): Ty {
  return {
    kind: "error",
    err: emitError(cx, err),
  };
}

export function tyErrorFrom(prev: { err: ErrorEmitted }): Ty {
  return { kind: "error", err: prev.err };
}

export function emitError(cx: TypeckCtx, err: CompilerError): ErrorEmitted {
  return cx.gcx.error.emit(err);
}

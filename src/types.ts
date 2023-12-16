import { ItemId, Resolution } from "./ast";
import { ErrorEmitted } from "./error";

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
  itemId: ItemId;
  params: string[];
  genericArgs: Ty[];
  _name: string;
  fields_no_subst: [string, Ty][];
};

export type TyRawPtr = {
  kind: "rawptr";
  inner: Ty;
};

export type TyNever = {
  kind: "never";
};

export type TyParam = {
  kind: "param";
  /**
   * The index of the type parameter of the parent.
   * If the parent is `type A[T, U] = U;`
   * then `U` will have index 1.
   */
  idx: number;
  name: string;
};

export type TyAlias = {
  kind: "alias";
  actual: Ty;
  genericArgs: Ty[];
  params: string[];
};

export type TyError = {
  kind: "error";
  err: ErrorEmitted;
};

export type Ty =
  | TyString
  | TyInt
  | TyI32
  | TyBool
  | TyTuple
  | TyFn
  | TyVar
  | TyStruct
  | TyRawPtr
  | TyNever
  | TyParam
  | TyAlias
  | TyError;

export function tyIsUnit(ty: Ty): ty is TyUnit {
  return ty.kind === "tuple" && ty.elems.length === 0;
}

export const TYS = {
  UNIT: { kind: "tuple", elems: [] } as Ty,
  STRING: { kind: "string" } as Ty,
  BOOL: { kind: "bool" } as Ty,
  INT: { kind: "int" } as Ty,
  I32: { kind: "i32" } as Ty,
  NEVER: { kind: "never" } as Ty,
} as const;

export type TypeckResults = {
  main: Resolution | undefined;
};

export function structFieldsSubstituted(ty: TyStruct): [string, Ty][] {
  const args = ty.genericArgs;
  return ty.fields_no_subst.map(([name, type]) => [
    name,
    substituteTy(args, type),
  ]);
}

// Substitute the parameter of a type. We are only able to handle one
// level of generic definitions, for example for fields the struct def or for exprs the function generics.
export function substituteTy(genericArgs: Ty[], ty: Ty): Ty {
  const subst = (ty: Ty) => substituteTy(genericArgs, ty);
  switch (ty.kind) {
    case "param":
      if (ty.idx >= genericArgs.length) {
        throw new Error(
          `substitution out of range, param index ${ty.idx} of param ${ty.name} out of range for length ${genericArgs.length}`,
        );
      }
      return genericArgs[ty.idx];
    case "tuple":
      return { ...ty, elems: ty.elems.map(subst) };
    case "fn":
      return {
        ...ty,
        returnTy: subst(ty.returnTy),
        params: ty.params.map(subst),
      };
    case "struct":
    case "alias":
      return {
        ...ty,
        genericArgs: ty.genericArgs.map(subst),
      };
    case "rawptr":
      return { ...ty, inner: subst(ty.inner) };
    // Primitives
    case "var":
    case "string":
    case "int":
    case "i32":
    case "bool":
    case "never":
    case "error":
      return ty;
  }
}

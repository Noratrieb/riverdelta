import { Pkg, DepPkg, Final, Item, ItemId, Phase } from "./ast";
import { ErrorHandler, Span } from "./error";
import { Options } from "./options";
import { Ids, unwrap } from "./utils";
export type PkgLoader = (
  gcx: GlobalContext,
  name: string,
  span: Span,
) => DepPkg;

/**
 * The global context containing information about the _global compilation session_,
 * like loaded pkgs.
 * Notably, the global context is _not_ supposed to have information specific to the "local pkg",
 * because with the current compilation model, there is no "local pkg" in a session.
 *
 * There is a "downstream"/"binary"/"final" pkg with pkgId=0, where `function main()` lives, but
 * dependencies (which also use the same context) do not care about that.
 */
export class GlobalContext {
  public error: ErrorHandler;
  public finalizedPkgs: Pkg<Final>[] = [];
  // For cycle detection.
  public pkgsBeingLoaded: Set<string> = new Set<string>();
  public pkgId: Ids = new Ids();

  constructor(public opts: Options, public pkgLoader: PkgLoader) {
    this.error = new ErrorHandler(opts);
  }

  public findItem<P extends Phase>(
    id: ItemId,
    localPkg?: Pkg<P>,
  ): Item<P> | Item<Final> {
    const allPkgs: (Pkg<P> | Pkg<Final>)[] = [
      ...(localPkg ? [localPkg] : []),
      ...this.finalizedPkgs,
    ];

    const pkg: Pkg<P> | Pkg<Final> = unwrap(
      allPkgs.find((pkg) => pkg.id === id.pkgId),
    );

    if (pkg.fatalError) {
      return {
        kind: "error",
        defPath: [],
        err: pkg.fatalError,
        id: new ItemId(pkg.id, 0),
        name: "",
        span: Span.startOfFile(pkg.rootFile),
      };
    }

    if (id.itemIdx === 0) {
      const contents: Item<P>[] | Item<Final>[] = pkg.rootItems;
      // Typescript does not seem to be able to understand this here.
      // The type of this is supposed to be (Item<P> | Item<Final>)["contents"] which is
      // "too complex to represent".
      const erasedContents: any = contents;

      // Return a synthetic module representing the pkg root.
      const mod: Item<P> | Item<Final> = {
        kind: "mod",
        name: pkg.packageName,
        contents: erasedContents,
        span: Span.startOfFile(pkg.rootFile),
        id,
      };
      return mod;
    }

    const mod = unwrap(pkg.itemsById.get(id));
    return mod;
  }
}

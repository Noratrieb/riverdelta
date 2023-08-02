import { Crate, DepCrate, Final, Item, ItemId, Phase } from "./ast";
import { DUMMY_SPAN, Span } from "./error";
import { Ids, unwrap } from "./utils";

export type CrateLoader = (
  gcx: GlobalContext,
  name: string,
  span: Span
) => DepCrate;

/**
 * The global context containing information about the _global compilation session_,
 * like loaded crates.
 * Notably, the global context is _not_ supposed to have information specific to the "local crate",
 * because with the current compilation model, there is no "local crate" in a session.
 *
 * There is a "downstream"/"binary"/"final" crate with crateId=0, where `function main()` lives, but
 * dependencies (which also use the same context) do not care about that.
 */
export class GlobalContext {
  public depCrates: Crate<Final>[] = [];
  public crateId: Ids = new Ids();

  constructor(public crateLoader: CrateLoader) {}

  public findItem<P extends Phase>(
    id: ItemId,
    localCrate: Crate<P>
  ): Item<P | Final> {
    const crate = unwrap(
      [localCrate, ...this.depCrates].find((crate) => crate.id === id.crateId)
    );

    if (id.itemIdx === 0) {
      // Return a synthetic module representing the crate root.
      return {
        kind: "mod",
        node: {
          contents: crate.rootItems,
          name: crate.packageName,
        },
        span: DUMMY_SPAN,
        id,
      };
    }

    return unwrap(crate.itemsById.get(id));
  }
}

/* @generated from adl module sys.package */


/**
 * Metadata for an ADL package
 */
export interface AdlPackage {
  name: string;
  dependencies: AdlPackageRef[];
}

export function makeAdlPackage(
  input: {
    name: string,
    dependencies?: AdlPackageRef[],
  }
): AdlPackage {
  return {
    name: input.name,
    dependencies: input.dependencies === undefined ? [] : input.dependencies,
  };
}

export interface AdlPackageRef_Stdlib {
  kind: 'stdlib';
}
export interface AdlPackageRef_Localdir {
  kind: 'localdir';
  value: string;
}

/**
 * A reference to an ADL package
 * potentially will be extended in future with
 * sources 
 */
export type AdlPackageRef = AdlPackageRef_Stdlib | AdlPackageRef_Localdir;

export interface AdlPackageRefOpts {
  /**
   * The stdlib package included in the ADL toolchain
   */
  stdlib: null;
  /**
   * A package stored in a directory in the local
   * filesystem. A relative path is relative to the
   * file containing the package definition.
   */
  localdir: string;
}

export function makeAdlPackageRef<K extends keyof AdlPackageRefOpts>(kind: K, value: AdlPackageRefOpts[K]) { return {kind, value}; }

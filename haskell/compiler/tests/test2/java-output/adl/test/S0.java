package adl.test;

import org.adl.runtime.Factory;

/**
 * An empty structure.
 */
public class S0 {


  /* Constructors */

  public S0() {
  }

  public S0(S0 other) {
  }

  /* Object level helpers */

  public boolean equals(S0 other) {
    return true;
  }

  public int hashCode() {
    int result = 1;
    return result;
  }

  /* Factory for construction of generic values */

  public static Factory<S0> factory = new Factory<S0>() {
    public S0 create() {
      return new S0();
    }
    public S0 create(S0 other) {
      return new S0(other);
    }
  };
}
package adl.test2;

import org.adl.runtime.Factory;
import java.util.Objects;

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

  @Override
  public boolean equals(Object other) {
    return other instanceof S0;
  }

  @Override
  public int hashCode() {
    int result = 1;
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<S0> FACTORY = new Factory<S0>() {
    public S0 create() {
      return new S0();
    }
    public S0 create(S0 other) {
      return new S0(other);
    }
  };
}

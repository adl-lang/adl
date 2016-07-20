package adl.test;

import org.adl.runtime.Factory;

public class S0 {


  public S0() {
  }

  public S0(S0 other) {
  }

  public boolean equals(S0 other) {
    return true;
  }

  public int hashCode() {
    int result = 1;
    return result;
  }

  public static Factory<S0> factory = new Factory<S0>() {
    public S0 create() {
      return new S0();
    }
    public S0 create(S0 other) {
      return new S0(other);
    }
  };
}
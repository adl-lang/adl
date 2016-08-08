package adl.test5;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import java.util.Objects;

public class S1 {

  /* Members */

  private short f;

  /* Constructors */

  public S1(short f) {
    this.f = f;
  }

  public S1() {
    this.f = (short)100;
  }

  public S1(S1 other) {
    this.f = other.f;
  }

  /* Accessors and mutators */

  public short getF() {
    return f;
  }

  public void setF(short newF) {
    f = newF;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S1)) {
      return false;
    }
    S1 other = (S1) other0;
    return
      f == other.f;
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + (int) f;
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<S1> FACTORY = new Factory<S1>() {
    public S1 create() {
      return new S1();
    }
    public S1 create(S1 other) {
      return new S1(other);
    }
  };
}

package adl.test5;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import java.util.Objects;

public class U2 {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The U2 discriminator type.
   */
  public enum Disc {
    V
  }

  /* Constructors */

  public static U2 v(short v) {
    return new U2(Disc.V, v);
  }

  public U2() {
    this.disc = Disc.V;
    this.value = 0;
  }

  public U2(U2 other) {
    this.disc = other.disc;
    switch (other.disc) {
      case V:
        this.value = (Short) other.value;
        break;
    }
  }

  private U2(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public short getV() {
    if (disc == Disc.V) {
      return (Short) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setV(short v) {
    this.value = v;
    this.disc = Disc.V;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof U2)) {
      return false;
    }
    U2 other = (U2) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<U2> FACTORY = new Factory<U2>() {
    public U2 create() {
      return new U2();
    }
    public U2 create(U2 other) {
      return new U2(other);
    }
  };
}

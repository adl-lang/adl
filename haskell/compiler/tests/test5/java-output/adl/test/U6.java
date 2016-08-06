package adl.test;

import org.adl.runtime.Factory;
import java.util.Objects;

public class U6 {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The U6 discriminator type.
   */
  public enum Disc {
    V
  }

  /* Constructors */

  public static U6 v(U3 v) {
    return new U6(Disc.V, Objects.requireNonNull(v));
  }

  public U6() {
    this.disc = Disc.V;
    this.value = new U3();
  }

  public U6(U6 other) {
    this.disc = other.disc;
    switch (other.disc) {
      case V:
        this.value = U3.FACTORY.create((U3) other.value);
        break;
    }
  }

  private U6(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public U3 getV() {
    if (disc == Disc.V) {
      return (U3) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setV(U3 v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.V;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof U6)) {
      return false;
    }
    U6 other = (U6) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<U6> FACTORY = new Factory<U6>() {
    public U6 create() {
      return new U6();
    }
    public U6 create(U6 other) {
      return new U6(other);
    }
  };
}

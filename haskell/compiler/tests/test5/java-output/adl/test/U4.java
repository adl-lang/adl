package adl.test;

import org.adl.runtime.Factory;

public class U4 {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The U4 discriminator type.
   */
  public enum Disc {
    V
  }

  /* Constructors */

  public static U4 v(S1 v) {
    return new U4(Disc.V, java.util.Objects.requireNonNull(v));
  }

  public U4() {
    this.disc = Disc.V;
    this.value = new S1();
  }

  public U4(U4 other) {
    this.disc = other.disc;
    switch (other.disc) {
      case V:
        this.value = S1.FACTORY.create(cast(other.value));
        break;
    }
  }

  private U4(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public S1 getV() {
    if (disc == Disc.V) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setV(S1 v) {
    this.value = java.util.Objects.requireNonNull(v);
    this.disc = Disc.V;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof U4)) {
      return false;
    }
    U4 other = (U4) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  @SuppressWarnings("unchecked")
  private static <T> T cast(final Object o) {
    return (T) o;
  }

  /* Factory for construction of generic values */

  public static final Factory<U4> FACTORY = new Factory<U4>() {
    public U4 create() {
      return new U4();
    }
    public U4 create(U4 other) {
      return new U4(other);
    }
  };
}

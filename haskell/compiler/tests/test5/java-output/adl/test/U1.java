package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class U1 {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The U1 discriminator type.
   */
  public enum Disc {
    V
  }

  /* Constructors */

  public static U1 v() {
    return new U1(Disc.V, null);
  }

  public U1() {
    this.disc = Disc.V;
    this.value = null;
  }

  public U1(U1 other) {
    this.disc = other.disc;
    switch (other.disc) {
      case V:
        this.value = (Void) other.value;
        break;
    }
  }

  private U1(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  /* Mutators */

  public void setV() {
    this.value = null;
    this.disc = Disc.V;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof U1)) {
      return false;
    }
    U1 other = (U1) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<U1> FACTORY = new Factory<U1>() {
    public U1 create() {
      return new U1();
    }
    public U1 create(U1 other) {
      return new U1(other);
    }
  };
}

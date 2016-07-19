package adl.test;

import org.adl.runtime.Factory;

public class U7 {

  private Disc disc;
  private Object value;

  public enum Disc {
    V
  }

  public static U7 v(U3 v) {
    return new U7(Disc.V,java.util.Objects.requireNonNull(v));
  }

  public U7() {
    this.disc = Disc.V;
    this.value = U3.v((short)75);
  }

  public U7(U7 other) {
    this.disc = other.disc;
    switch (other.disc) {
      case V:
        this.value = U3.factory.create((U3) other.value);
        break;
    }
  }

  private U7(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  public Disc getDisc() {
    return disc;
  }

  public U3 getV() {
    if (disc == Disc.V) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  public void setV(U3 v) {
    this.value = java.util.Objects.requireNonNull(v);
    this.disc = Disc.V;
  }

  public boolean equals(U7 other) {
    return disc == other.disc && value.equals(other.value);
  }

  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  @SuppressWarnings("unchecked")
  private static <T> T cast(final Object o) {
    return (T)o;
  }

  public static Factory<U7> factory = new Factory<U7>() {
    public U7 create() {
      return new U7();
    }
    public U7 create(U7 other) {
      return new U7(other);
    }
  };
}
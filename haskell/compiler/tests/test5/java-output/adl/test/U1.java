package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class U1 {

  private Disc disc;
  private Object value;

  public enum Disc {
    V
  }

  public static U1 v(Void v) {
    return new U1(Disc.V,v);
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

  public Disc getDisc() {
    return disc;
  }

  public Void getV() {
    if (disc == Disc.V) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  public void setV(Void v) {
    this.value = v;
    this.disc = Disc.V;
  }

  public boolean equals(U1 other) {
    return disc == other.disc && value.equals(other.value);
  }

  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  @SuppressWarnings("unchecked")
  private static <T> T cast(final Object o) {
    return (T)o;
  }

  public static Factory<U1> factory = new Factory<U1>() {
    public U1 create() {
      return new U1();
    }
    public U1 create(U1 other) {
      return new U1(other);
    }
  };
}
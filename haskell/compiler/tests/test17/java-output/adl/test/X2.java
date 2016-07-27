package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class X2 {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The X2 discriminator type.
   */
  public enum Disc {
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7
  }

  /* Constructors */

  public static X2 f1(int v) {
    return new X2(Disc.F1, v);
  }

  public static X2 f2(int v) {
    return new X2(Disc.F2, v);
  }

  public static X2 f3(Pair<String, Integer> v) {
    return new X2(Disc.F3, java.util.Objects.requireNonNull(v));
  }

  public static X2 f4(Pair<String, String> v) {
    return new X2(Disc.F4, java.util.Objects.requireNonNull(v));
  }

  public static X2 f5(java.util.ArrayList<Integer> v) {
    return new X2(Disc.F5, java.util.Objects.requireNonNull(v));
  }

  public static X2 f6(java.util.ArrayList<Pair<String, Integer>> v) {
    return new X2(Disc.F6, java.util.Objects.requireNonNull(v));
  }

  public static X2 f7(java.util.ArrayList<Pair<String, String>> v) {
    return new X2(Disc.F7, java.util.Objects.requireNonNull(v));
  }

  public X2() {
    this.disc = Disc.F1;
    this.value = 0;
  }

  public X2(X2 other) {
    this.disc = other.disc;
    switch (other.disc) {
      case F1:
        this.value = (Integer) other.value;
        break;
      case F2:
        this.value = (Integer) other.value;
        break;
      case F3:
        this.value = Pair.factory(Factories.STRING, Factories.INTEGER).create((Pair<String, Integer>) other.value);
        break;
      case F4:
        this.value = Pair.factory(Factories.STRING, Factories.STRING).create((Pair<String, String>) other.value);
        break;
      case F5:
        this.value = Factories.arrayList(Factories.INTEGER).create((java.util.ArrayList<Integer>) other.value);
        break;
      case F6:
        this.value = Factories.arrayList(Pair.factory(Factories.STRING, Factories.INTEGER)).create((java.util.ArrayList<Pair<String, Integer>>) other.value);
        break;
      case F7:
        this.value = Factories.arrayList(Pair.factory(Factories.STRING, Factories.STRING)).create((java.util.ArrayList<Pair<String, String>>) other.value);
        break;
    }
  }

  private X2(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public int getF1() {
    if (disc == Disc.F1) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  public int getF2() {
    if (disc == Disc.F2) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  public Pair<String, Integer> getF3() {
    if (disc == Disc.F3) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  public Pair<String, String> getF4() {
    if (disc == Disc.F4) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  public java.util.ArrayList<Integer> getF5() {
    if (disc == Disc.F5) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  public java.util.ArrayList<Pair<String, Integer>> getF6() {
    if (disc == Disc.F6) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  public java.util.ArrayList<Pair<String, String>> getF7() {
    if (disc == Disc.F7) {
      return cast(value);
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setF1(int v) {
    this.value = v;
    this.disc = Disc.F1;
  }

  public void setF2(int v) {
    this.value = v;
    this.disc = Disc.F2;
  }

  public void setF3(Pair<String, Integer> v) {
    this.value = java.util.Objects.requireNonNull(v);
    this.disc = Disc.F3;
  }

  public void setF4(Pair<String, String> v) {
    this.value = java.util.Objects.requireNonNull(v);
    this.disc = Disc.F4;
  }

  public void setF5(java.util.ArrayList<Integer> v) {
    this.value = java.util.Objects.requireNonNull(v);
    this.disc = Disc.F5;
  }

  public void setF6(java.util.ArrayList<Pair<String, Integer>> v) {
    this.value = java.util.Objects.requireNonNull(v);
    this.disc = Disc.F6;
  }

  public void setF7(java.util.ArrayList<Pair<String, String>> v) {
    this.value = java.util.Objects.requireNonNull(v);
    this.disc = Disc.F7;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof X2)) {
      return false;
    }
    X2 other = (X2) other0;
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

  public static final Factory<X2> FACTORY = new Factory<X2>() {
    public X2 create() {
      return new X2();
    }
    public X2 create(X2 other) {
      return new X2(other);
    }
  };
}

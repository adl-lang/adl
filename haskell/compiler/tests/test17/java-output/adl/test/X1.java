package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class X1 {

  /* Members */

  private int f1;
  private int f2;
  private Pair<String, Integer> f3;
  private Pair<String, String> f4;
  private java.util.ArrayList<Integer> f5;
  private java.util.ArrayList<Pair<String, Integer>> f6;
  private java.util.ArrayList<Pair<String, String>> f7;

  /* Constructors */

  public X1(int f1, int f2, Pair<String, Integer> f3, Pair<String, String> f4, java.util.ArrayList<Integer> f5, java.util.ArrayList<Pair<String, Integer>> f6, java.util.ArrayList<Pair<String, String>> f7) {
    this.f1 = f1;
    this.f2 = f2;
    this.f3 = java.util.Objects.requireNonNull(f3);
    this.f4 = java.util.Objects.requireNonNull(f4);
    this.f5 = java.util.Objects.requireNonNull(f5);
    this.f6 = java.util.Objects.requireNonNull(f6);
    this.f7 = java.util.Objects.requireNonNull(f7);
  }

  public X1() {
    this.f1 = 0;
    this.f2 = 0;
    this.f3 = Pair.factory(Factories.STRING, Factories.INTEGER).create();
    this.f4 = Pair.factory(Factories.STRING, Factories.STRING).create();
    this.f5 = new java.util.ArrayList<Integer>();
    this.f6 = new java.util.ArrayList<Pair<String, Integer>>();
    this.f7 = new java.util.ArrayList<Pair<String, String>>();
  }

  public X1(X1 other) {
    this.f1 = other.f1;
    this.f2 = other.f2;
    this.f3 = Pair.factory(Factories.STRING, Factories.INTEGER).create(other.f3);
    this.f4 = Pair.factory(Factories.STRING, Factories.STRING).create(other.f4);
    this.f5 = Factories.arrayList(Factories.INTEGER).create(other.f5);
    this.f6 = Factories.arrayList(Pair.factory(Factories.STRING, Factories.INTEGER)).create(other.f6);
    this.f7 = Factories.arrayList(Pair.factory(Factories.STRING, Factories.STRING)).create(other.f7);
  }

  /* Accessors and mutators */

  public int getF1() {
    return f1;
  }

  public void setF1(int newF1) {
    f1 = newF1;
  }

  public int getF2() {
    return f2;
  }

  public void setF2(int newF2) {
    f2 = newF2;
  }

  public Pair<String, Integer> getF3() {
    return f3;
  }

  public void setF3(Pair<String, Integer> newF3) {
    f3 = java.util.Objects.requireNonNull(newF3);
  }

  public Pair<String, String> getF4() {
    return f4;
  }

  public void setF4(Pair<String, String> newF4) {
    f4 = java.util.Objects.requireNonNull(newF4);
  }

  public java.util.ArrayList<Integer> getF5() {
    return f5;
  }

  public void setF5(java.util.ArrayList<Integer> newF5) {
    f5 = java.util.Objects.requireNonNull(newF5);
  }

  public java.util.ArrayList<Pair<String, Integer>> getF6() {
    return f6;
  }

  public void setF6(java.util.ArrayList<Pair<String, Integer>> newF6) {
    f6 = java.util.Objects.requireNonNull(newF6);
  }

  public java.util.ArrayList<Pair<String, String>> getF7() {
    return f7;
  }

  public void setF7(java.util.ArrayList<Pair<String, String>> newF7) {
    f7 = java.util.Objects.requireNonNull(newF7);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof X1)) {
      return false;
    }
    X1 other = (X1) other0;
    return
      f1 == other.f1 &&
      f2 == other.f2 &&
      f3.equals(other.f3) &&
      f4.equals(other.f4) &&
      f5.equals(other.f5) &&
      f6.equals(other.f6) &&
      f7.equals(other.f7);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + f1;
    result = result * 37 + f2;
    result = result * 37 + f3.hashCode();
    result = result * 37 + f4.hashCode();
    result = result * 37 + f5.hashCode();
    result = result * 37 + f6.hashCode();
    result = result * 37 + f7.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<X1> FACTORY = new Factory<X1>() {
    public X1 create() {
      return new X1();
    }
    public X1 create(X1 other) {
      return new X1(other);
    }
  };
}

package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class S2 {

  /* Members */

  private String f1;
  private double f2;
  private java.util.ArrayList<Integer> f3;

  /* Constructors */

  public S2(String f1, double f2, java.util.ArrayList<Integer> f3) {
    this.f1 = java.util.Objects.requireNonNull(f1);
    this.f2 = f2;
    this.f3 = java.util.Objects.requireNonNull(f3);
  }

  public S2() {
    this.f1 = "";
    this.f2 = 0.0;
    this.f3 = new java.util.ArrayList<Integer>();
  }

  public S2(S2 other) {
    this.f1 = other.f1;
    this.f2 = other.f2;
    this.f3 = Factories.ArrayListFactory(Factories.IntegerFactory).create(other.f3);
  }

  /* Accessors and mutators */

  public String getF1() {
    return f1;
  }

  public void setF1(String newF1) {
    f1 = newF1;
  }

  public double getF2() {
    return f2;
  }

  public void setF2(double newF2) {
    f2 = newF2;
  }

  public java.util.ArrayList<Integer> getF3() {
    return f3;
  }

  public void setF3(java.util.ArrayList<Integer> newF3) {
    f3 = newF3;
  }

  /* Object level helpers */

  public boolean equals(S2 other) {
    return
      f1.equals(other.f1) &&
      f2 == other.f2 &&
      f3.equals(other.f3);
  }

  public int hashCode() {
    int result = 1;
    result = result * 37 + f1.hashCode();
    result = result * 37 + Double.hashCode(f2);
    result = result * 37 + f3.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static Factory<S2> factory = new Factory<S2>() {
    public S2 create() {
      return new S2();
    }
    public S2 create(S2 other) {
      return new S2(other);
    }
  };
}
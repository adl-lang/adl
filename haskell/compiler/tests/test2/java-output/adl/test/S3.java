package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class S3<T> {

  private String f1;
  private double f2;
  private T f3;
  private java.util.ArrayList<T> f4;

  public S3(String f1, double f2, T f3, java.util.ArrayList<T> f4) {
    this.f1 = java.util.Objects.requireNonNull(f1);
    this.f2 = f2;
    this.f3 = java.util.Objects.requireNonNull(f3);
    this.f4 = java.util.Objects.requireNonNull(f4);
  }

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

  public T getF3() {
    return f3;
  }

  public void setF3(T newF3) {
    f3 = newF3;
  }

  public java.util.ArrayList<T> getF4() {
    return f4;
  }

  public void setF4(java.util.ArrayList<T> newF4) {
    f4 = newF4;
  }

  public boolean equals(S3 other) {
    return
      f1.equals(other.f1) &&
      f2 == other.f2 &&
      f3.equals(other.f3) &&
      f4.equals(other.f4);
  }

  public int hashCode() {
    int result = 1;
    result = result * 37 + f1.hashCode();
    result = result * 37 + (int)f2;
    result = result * 37 + f3.hashCode();
    result = result * 37 + f4.hashCode();
    return result;
  }

  public static <T> Factory<S3<T>> factory(Factory<T> factoryT) {
    return new Factory<S3<T>>() {
      final Factory<T> f3 = factoryT;
      final Factory<java.util.ArrayList<T>> f4 = Factories.ArrayListFactory(factoryT);

      public S3<T> create() {
        return new S3<T>("",0,f3.create(),f4.create());
      }

      public S3<T> create(S3<T> other) {
        return new S3<T>(other.getF1(),other.getF2(),f3.create(other.getF3()),f4.create(other.getF4()));
      }
    };
  }
}
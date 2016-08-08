package adl.test2;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import java.util.ArrayList;
import java.util.Objects;

/**
 * A generic structure.
 */
public class S3<T> {

  /* Members */

  private String f1;
  private double f2;
  private T f3;
  private ArrayList<T> f4;

  /* Constructors */

  public S3(String f1, double f2, T f3, ArrayList<T> f4) {
    this.f1 = Objects.requireNonNull(f1);
    this.f2 = f2;
    this.f3 = Objects.requireNonNull(f3);
    this.f4 = Objects.requireNonNull(f4);
  }

  /* Accessors and mutators */

  public String getF1() {
    return f1;
  }

  public void setF1(String newF1) {
    f1 = Objects.requireNonNull(newF1);
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
    f3 = Objects.requireNonNull(newF3);
  }

  public ArrayList<T> getF4() {
    return f4;
  }

  public void setF4(ArrayList<T> newF4) {
    f4 = Objects.requireNonNull(newF4);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S3)) {
      return false;
    }
    S3 other = (S3) other0;
    return
      f1.equals(other.f1) &&
      f2 == other.f2 &&
      f3.equals(other.f3) &&
      f4.equals(other.f4);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + f1.hashCode();
    result = result * 37 + Double.valueOf(f2).hashCode();
    result = result * 37 + f3.hashCode();
    result = result * 37 + f4.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static <T> Factory<S3<T>> factory(Factory<T> factoryT) {
    return new Factory<S3<T>>() {
      final Factory<T> f3 = factoryT;
      final Factory<ArrayList<T>> f4 = Factories.arrayList(factoryT);

      public S3<T> create() {
        return new S3<T>("", 0.0, f3.create(), f4.create());
      }

      public S3<T> create(S3<T> other) {
        return new S3<T>(other.getF1(), other.getF2(), f3.create(other.getF3()), f4.create(other.getF4()));
      }
    };
  }
}

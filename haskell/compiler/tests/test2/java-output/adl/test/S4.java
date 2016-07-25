package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class S4<T> {

  /* Members */

  private S3<String> f1;
  private S3<T> f2;

  /* Constructors */

  public S4(S3<String> f1, S3<T> f2) {
    this.f1 = java.util.Objects.requireNonNull(f1);
    this.f2 = java.util.Objects.requireNonNull(f2);
  }

  /* Accessors and mutators */

  public S3<String> getF1() {
    return f1;
  }

  public void setF1(S3<String> newF1) {
    f1 = newF1;
  }

  public S3<T> getF2() {
    return f2;
  }

  public void setF2(S3<T> newF2) {
    f2 = newF2;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S4)) {
      return false;
    }
    S4 other = (S4)other0;
    return
      f1.equals(other.f1) &&
      f2.equals(other.f2);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + f1.hashCode();
    result = result * 37 + f2.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static <T> Factory<S4<T>> factory(Factory<T> factoryT) {
    return new Factory<S4<T>>() {
      final Factory<S3<String>> f1 = S3.factory(Factories.StringFactory);
      final Factory<S3<T>> f2 = S3.factory(factoryT);

      public S4<T> create() {
        return new S4<T>(f1.create(), f2.create());
      }

      public S4<T> create(S4<T> other) {
        return new S4<T>(f1.create(other.getF1()), f2.create(other.getF2()));
      }
    };
  }
}
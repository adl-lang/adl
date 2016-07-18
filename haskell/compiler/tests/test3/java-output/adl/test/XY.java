package adl.test;

import org.adl.runtime.Factory;

public class XY<T> {

  private T x;
  private T y;

  public XY(T x, T y) {
    this.x = java.util.Objects.requireNonNull(x);
    this.y = java.util.Objects.requireNonNull(y);
  }

  public T getX() {
    return x;
  }

  public void setX(T newX) {
    x = newX;
  }

  public T getY() {
    return y;
  }

  public void setY(T newY) {
    y = newY;
  }

  public boolean equals(XY other) {
    return
      x.equals(other.x) &&
      y.equals(other.y);
  }

  public int hashCode() {
    int result = 1;
    result = result * 37 + x.hashCode();
    result = result * 37 + y.hashCode();
    return result;
  }

  public static <T> Factory<XY<T>> factory(Factory<T> factoryT) {
    return new Factory<XY<T>>() {
      final Factory<T> x = factoryT;
      final Factory<T> y = factoryT;

      public XY<T> create() {
        return new XY<T>(x.create(),y.create());
      }

      public XY<T> create(XY<T> other) {
        return new XY<T>(x.create(other.getX()),y.create(other.getY()));
      }
    };
  }
}
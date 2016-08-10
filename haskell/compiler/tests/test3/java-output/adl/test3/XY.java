package adl.test3;

import org.adl.runtime.Factory;
import java.util.Objects;

public class XY<T> {

  /* Members */

  private T x;
  private T y;

  /* Constructors */

  public XY(T x, T y) {
    this.x = Objects.requireNonNull(x);
    this.y = Objects.requireNonNull(y);
  }

  /* Accessors and mutators */

  public T getX() {
    return x;
  }

  public void setX(T newX) {
    x = Objects.requireNonNull(newX);
  }

  public T getY() {
    return y;
  }

  public void setY(T newY) {
    y = Objects.requireNonNull(newY);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof XY)) {
      return false;
    }
    XY other = (XY) other0;
    return
      x.equals(other.x) &&
      y.equals(other.y);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + x.hashCode();
    result = result * 37 + y.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static <T> Factory<XY<T>> factory(Factory<T> factoryT) {
    return new Factory<XY<T>>() {
      final Factory<T> x = factoryT;
      final Factory<T> y = factoryT;

      public XY<T> create() {
        return new XY<T>(
          x.create(),
          y.create()
          );
      }

      public XY<T> create(XY<T> other) {
        return new XY<T>(
          x.create(other.getX()),
          y.create(other.getY())
          );
      }
    };
  }
}

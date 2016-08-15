package adl.test3;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import java.util.ArrayList;
import java.util.Objects;

public class B<T> {

  /* Members */

  private T f_t;
  private String f_string;
  private ArrayList<T> f_tvec;
  private XY<T> f_xy;

  /* Constructors */

  public B(T f_t, String f_string, ArrayList<T> f_tvec, XY<T> f_xy) {
    this.f_t = Objects.requireNonNull(f_t);
    this.f_string = Objects.requireNonNull(f_string);
    this.f_tvec = Objects.requireNonNull(f_tvec);
    this.f_xy = Objects.requireNonNull(f_xy);
  }

  /* Accessors and mutators */

  public T getF_t() {
    return f_t;
  }

  public void setF_t(T newF_t) {
    f_t = Objects.requireNonNull(newF_t);
  }

  public String getF_string() {
    return f_string;
  }

  public void setF_string(String newF_string) {
    f_string = Objects.requireNonNull(newF_string);
  }

  public ArrayList<T> getF_tvec() {
    return f_tvec;
  }

  public void setF_tvec(ArrayList<T> newF_tvec) {
    f_tvec = Objects.requireNonNull(newF_tvec);
  }

  public XY<T> getF_xy() {
    return f_xy;
  }

  public void setF_xy(XY<T> newF_xy) {
    f_xy = Objects.requireNonNull(newF_xy);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof B)) {
      return false;
    }
    B other = (B) other0;
    return
      f_t.equals(other.f_t) &&
      f_string.equals(other.f_string) &&
      f_tvec.equals(other.f_tvec) &&
      f_xy.equals(other.f_xy);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + f_t.hashCode();
    result = result * 37 + f_string.hashCode();
    result = result * 37 + f_tvec.hashCode();
    result = result * 37 + f_xy.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static <T> Factory<B<T>> factory(Factory<T> factoryT) {
    return new Factory<B<T>>() {
      final Factory<T> f_t = factoryT;
      final Factory<String> f_string = Factories.STRING;
      final Factory<ArrayList<T>> f_tvec = Factories.arrayList(factoryT);
      final Factory<XY<T>> f_xy = XY.factory(factoryT);

      public B<T> create() {
        return new B<T>(
          f_t.create(),
          f_string.create(),
          f_tvec.create(),
          f_xy.create()
          );
      }

      public B<T> create(B<T> other) {
        return new B<T>(
          f_t.create(other.getF_t()),
          other.getF_string(),
          f_tvec.create(other.getF_tvec()),
          f_xy.create(other.getF_xy())
          );
      }
    };
  }
}

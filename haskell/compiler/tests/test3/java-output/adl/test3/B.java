package adl.test3;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
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

  public void setF_t(T f_t) {
    this.f_t = Objects.requireNonNull(f_t);
  }

  public String getF_string() {
    return f_string;
  }

  public void setF_string(String f_string) {
    this.f_string = Objects.requireNonNull(f_string);
  }

  public ArrayList<T> getF_tvec() {
    return f_tvec;
  }

  public void setF_tvec(ArrayList<T> f_tvec) {
    this.f_tvec = Objects.requireNonNull(f_tvec);
  }

  public XY<T> getF_xy() {
    return f_xy;
  }

  public void setF_xy(XY<T> f_xy) {
    this.f_xy = Objects.requireNonNull(f_xy);
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

  /* Json serialization */

  public static<T> JsonBinding<B<T>> jsonBinding(JsonBinding<T> bindingT) {
    final JsonBinding<T> f_t = bindingT;
    final JsonBinding<String> f_string = JsonBindings.STRING;
    final JsonBinding<ArrayList<T>> f_tvec = JsonBindings.arrayList(bindingT);
    final JsonBinding<XY<T>> f_xy = XY.jsonBinding(bindingT);
    final Factory<T> factoryT = bindingT.factory();
    final Factory<B<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<B<T>>() {
      public Factory<B<T>> factory() {
        return _factory;
      }

      public JsonElement toJson(B<T> _value) {
        JsonObject _result = new JsonObject();
        _result.add("f_t", f_t.toJson(_value.f_t));
        _result.add("f_string", f_string.toJson(_value.f_string));
        _result.add("f_tvec", f_tvec.toJson(_value.f_tvec));
        _result.add("f_xy", f_xy.toJson(_value.f_xy));
        return _result;
      }

      public B<T> fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new B<T>(
          _obj.has("f_t") ? f_t.fromJson(_obj.get("f_t")) : factoryT.create(),
          _obj.has("f_string") ? f_string.fromJson(_obj.get("f_string")) : "",
          _obj.has("f_tvec") ? f_tvec.fromJson(_obj.get("f_tvec")) : new ArrayList<T>(),
          _obj.has("f_xy") ? f_xy.fromJson(_obj.get("f_xy")) : XY.factory(factoryT).create()
        );
      }
    };
  }
}

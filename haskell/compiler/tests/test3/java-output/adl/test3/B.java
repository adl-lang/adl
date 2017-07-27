/* Code generated from adl module test3 */

package adl.test3;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.sys.adlast.ScopedName;
import org.adl.sys.adlast.TypeExpr;
import org.adl.sys.adlast.TypeRef;
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
    B<?> other = (B<?>) other0;
    return
      f_t.equals(other.f_t) &&
      f_string.equals(other.f_string) &&
      f_tvec.equals(other.f_tvec) &&
      f_xy.equals(other.f_xy);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + f_t.hashCode();
    _result = _result * 37 + f_string.hashCode();
    _result = _result * 37 + f_tvec.hashCode();
    _result = _result * 37 + f_xy.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static <T> Factory<B<T>> factory(Factory<T> factoryT) {
    return new Factory<B<T>>() {
      final Lazy<Factory<T>> f_t = new Lazy<>(() -> factoryT);
      final Lazy<Factory<String>> f_string = new Lazy<>(() -> Factories.STRING);
      final Lazy<Factory<ArrayList<T>>> f_tvec = new Lazy<>(() -> Factories.arrayList(factoryT));
      final Lazy<Factory<XY<T>>> f_xy = new Lazy<>(() -> XY.factory(factoryT));

      @Override
      public B<T> create() {
        return new B<T>(
          f_t.get().create(),
          f_string.get().create(),
          f_tvec.get().create(),
          f_xy.get().create()
          );
      }

      @Override
      public B<T> create(B<T> other) {
        return new B<T>(
          f_t.get().create(other.getF_t()),
          other.getF_string(),
          f_tvec.get().create(other.getF_tvec()),
          f_xy.get().create(other.getF_xy())
          );
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("test3", "B");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryT.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<B<T>> jsonBinding(JsonBinding<T> bindingT) {
    final Lazy<JsonBinding<T>> f_t = new Lazy<>(() -> bindingT);
    final Lazy<JsonBinding<String>> f_string = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<ArrayList<T>>> f_tvec = new Lazy<>(() -> JsonBindings.arrayList(bindingT));
    final Lazy<JsonBinding<XY<T>>> f_xy = new Lazy<>(() -> XY.jsonBinding(bindingT));
    final Factory<T> factoryT = bindingT.factory();
    final Factory<B<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<B<T>>() {
      @Override
      public Factory<B<T>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(B<T> _value) {
        JsonObject _result = new JsonObject();
        _result.add("f_t", f_t.get().toJson(_value.f_t));
        _result.add("f_string", f_string.get().toJson(_value.f_string));
        _result.add("f_tvec", f_tvec.get().toJson(_value.f_tvec));
        _result.add("f_xy", f_xy.get().toJson(_value.f_xy));
        return _result;
      }

      @Override
      public B<T> fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new B<T>(
          JsonBindings.fieldFromJson(_obj, "f_t", f_t.get()),
          JsonBindings.fieldFromJson(_obj, "f_string", f_string.get()),
          JsonBindings.fieldFromJson(_obj, "f_tvec", f_tvec.get()),
          JsonBindings.fieldFromJson(_obj, "f_xy", f_xy.get())
        );
      }
    };
  }
}

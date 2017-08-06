/* Code generated from adl module test2 */

package adl.test2;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
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

  public void setF1(String f1) {
    this.f1 = Objects.requireNonNull(f1);
  }

  public double getF2() {
    return f2;
  }

  public void setF2(double f2) {
    this.f2 = f2;
  }

  public T getF3() {
    return f3;
  }

  public void setF3(T f3) {
    this.f3 = Objects.requireNonNull(f3);
  }

  public ArrayList<T> getF4() {
    return f4;
  }

  public void setF4(ArrayList<T> f4) {
    this.f4 = Objects.requireNonNull(f4);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S3)) {
      return false;
    }
    S3<?> other = (S3<?>) other0;
    return
      f1.equals(other.f1) &&
      f2 == other.f2 &&
      f3.equals(other.f3) &&
      f4.equals(other.f4);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + f1.hashCode();
    _result = _result * 37 + Double.valueOf(f2).hashCode();
    _result = _result * 37 + f3.hashCode();
    _result = _result * 37 + f4.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static <T> Factory<S3<T>> factory(Factory<T> factoryT) {
    return new Factory<S3<T>>() {
      final Lazy<Factory<String>> f1 = new Lazy<>(() -> Factories.STRING);
      final Lazy<Factory<Double>> f2 = new Lazy<>(() -> Factories.DOUBLE);
      final Lazy<Factory<T>> f3 = new Lazy<>(() -> factoryT);
      final Lazy<Factory<ArrayList<T>>> f4 = new Lazy<>(() -> Factories.arrayList(factoryT));

      @Override
      public S3<T> create() {
        return new S3<T>(
          f1.get().create(),
          f2.get().create(),
          f3.get().create(),
          f4.get().create()
          );
      }

      @Override
      public S3<T> create(S3<T> other) {
        return new S3<T>(
          other.getF1(),
          other.getF2(),
          f3.get().create(other.getF3()),
          f4.get().create(other.getF4())
          );
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("test2", "S3");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryT.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<S3<T>> jsonBinding(JsonBinding<T> bindingT) {
    final Lazy<JsonBinding<String>> f1 = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<Double>> f2 = new Lazy<>(() -> JsonBindings.DOUBLE);
    final Lazy<JsonBinding<T>> f3 = new Lazy<>(() -> bindingT);
    final Lazy<JsonBinding<ArrayList<T>>> f4 = new Lazy<>(() -> JsonBindings.arrayList(bindingT));
    final Factory<T> factoryT = bindingT.factory();
    final Factory<S3<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<S3<T>>() {
      @Override
      public Factory<S3<T>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(S3<T> _value) {
        JsonObject _result = new JsonObject();
        _result.add("f1", f1.get().toJson(_value.f1));
        _result.add("f2", f2.get().toJson(_value.f2));
        _result.add("f3", f3.get().toJson(_value.f3));
        _result.add("f4", f4.get().toJson(_value.f4));
        return _result;
      }

      @Override
      public S3<T> fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new S3<T>(
          JsonBindings.fieldFromJson(_obj, "f1", f1.get()),
          JsonBindings.fieldFromJson(_obj, "f2", f2.get()),
          JsonBindings.fieldFromJson(_obj, "f3", f3.get()),
          JsonBindings.fieldFromJson(_obj, "f4", f4.get())
        );
      }
    };
  }
}

/* @generated from adl module test3 */

package adl.test3;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Builders;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
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

  public XY<T> setX(T x) {
    this.x = Objects.requireNonNull(x);
    return this;
  }

  public T getY() {
    return y;
  }

  public XY<T> setY(T y) {
    this.y = Objects.requireNonNull(y);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof XY)) {
      return false;
    }
    XY<?> other = (XY<?>) other0;
    return
      x.equals(other.x) &&
      y.equals(other.y);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + x.hashCode();
    _result = _result * 37 + y.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder<T> {
    private T x;
    private T y;

    public Builder() {
      this.x = null;
      this.y = null;
    }

    public Builder<T> setX(T x) {
      this.x = Objects.requireNonNull(x);
      return this;
    }

    public Builder<T> setY(T y) {
      this.y = Objects.requireNonNull(y);
      return this;
    }

    public XY<T> create() {
      Builders.checkFieldInitialized("XY", "x", x);
      Builders.checkFieldInitialized("XY", "y", y);
      return new XY<T>(x, y);
    }
  }

  /* Factory for construction of generic values */

  public static <T> Factory<XY<T>> factory(Factory<T> factoryT) {
    return new Factory<XY<T>>() {
      final Lazy<Factory<T>> x = new Lazy<>(() -> factoryT);
      final Lazy<Factory<T>> y = new Lazy<>(() -> factoryT);

      @Override
      public XY<T> create(XY<T> other) {
        return new XY<T>(
          x.get().create(other.getX()),
          y.get().create(other.getY())
          );
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("test3", "XY");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryT.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }

      @Override
      public JsonBinding<XY<T>> jsonBinding() {
        return XY.jsonBinding(factoryT.jsonBinding());
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<XY<T>> jsonBinding(JsonBinding<T> bindingT) {
    final Lazy<JsonBinding<T>> x = new Lazy<>(() -> bindingT);
    final Lazy<JsonBinding<T>> y = new Lazy<>(() -> bindingT);
    final Factory<T> factoryT = bindingT.factory();
    final Factory<XY<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<XY<T>>() {
      @Override
      public Factory<XY<T>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(XY<T> _value) {
        JsonObject _result = new JsonObject();
        _result.add("x", x.get().toJson(_value.x));
        _result.add("y", y.get().toJson(_value.y));
        return _result;
      }

      @Override
      public XY<T> fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new XY<T>(
          JsonBindings.fieldFromJson(_obj, "x", x.get()),
          JsonBindings.fieldFromJson(_obj, "y", y.get())
        );
      }
    };
  }
}

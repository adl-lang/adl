/* Code generated from adl module picture */

package adl.picture;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Builders;
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

public class Translated<T> {

  /* Members */

  private double xoffset;
  private double yoffset;
  private T object;

  /* Constructors */

  public Translated(double xoffset, double yoffset, T object) {
    this.xoffset = xoffset;
    this.yoffset = yoffset;
    this.object = Objects.requireNonNull(object);
  }

  /* Accessors and mutators */

  public double getXoffset() {
    return xoffset;
  }

  public void setXoffset(double xoffset) {
    this.xoffset = xoffset;
  }

  public double getYoffset() {
    return yoffset;
  }

  public void setYoffset(double yoffset) {
    this.yoffset = yoffset;
  }

  public T getObject() {
    return object;
  }

  public void setObject(T object) {
    this.object = Objects.requireNonNull(object);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Translated)) {
      return false;
    }
    Translated<?> other = (Translated<?>) other0;
    return
      xoffset == other.xoffset &&
      yoffset == other.yoffset &&
      object.equals(other.object);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + Double.valueOf(xoffset).hashCode();
    _result = _result * 37 + Double.valueOf(yoffset).hashCode();
    _result = _result * 37 + object.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder<T> {
    private Double xoffset;
    private Double yoffset;
    private T object;

    public Builder() {
      this.xoffset = 0D;
      this.yoffset = 0D;
      this.object = null;
    }

    public Builder<T> setXoffset(Double xoffset) {
      this.xoffset = Objects.requireNonNull(xoffset);
      return this;
    }

    public Builder<T> setYoffset(Double yoffset) {
      this.yoffset = Objects.requireNonNull(yoffset);
      return this;
    }

    public Builder<T> setObject(T object) {
      this.object = Objects.requireNonNull(object);
      return this;
    }

    public Translated<T> create() {
      Builders.checkFieldInitialized("Translated", "object", object);
      return new Translated<T>(xoffset, yoffset, object);
    }
  }

  /* Factory for construction of generic values */

  public static <T> Factory<Translated<T>> factory(Factory<T> factoryT) {
    return new Factory<Translated<T>>() {
      final Lazy<Factory<Double>> xoffset = new Lazy<>(() -> Factories.DOUBLE);
      final Lazy<Factory<Double>> yoffset = new Lazy<>(() -> Factories.DOUBLE);
      final Lazy<Factory<T>> object = new Lazy<>(() -> factoryT);

      @Override
      public Translated<T> create() {
        return new Translated<T>(
          0D,
          0D,
          object.get().create()
          );
      }

      @Override
      public Translated<T> create(Translated<T> other) {
        return new Translated<T>(
          other.getXoffset(),
          other.getYoffset(),
          object.get().create(other.getObject())
          );
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("picture", "Translated");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryT.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<Translated<T>> jsonBinding(JsonBinding<T> bindingT) {
    final Lazy<JsonBinding<Double>> xoffset = new Lazy<>(() -> JsonBindings.DOUBLE);
    final Lazy<JsonBinding<Double>> yoffset = new Lazy<>(() -> JsonBindings.DOUBLE);
    final Lazy<JsonBinding<T>> object = new Lazy<>(() -> bindingT);
    final Factory<T> factoryT = bindingT.factory();
    final Factory<Translated<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<Translated<T>>() {
      @Override
      public Factory<Translated<T>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Translated<T> _value) {
        JsonObject _result = new JsonObject();
        _result.add("xoffset", xoffset.get().toJson(_value.xoffset));
        _result.add("yoffset", yoffset.get().toJson(_value.yoffset));
        _result.add("object", object.get().toJson(_value.object));
        return _result;
      }

      @Override
      public Translated<T> fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new Translated<T>(
          _obj.has("xoffset") ? JsonBindings.fieldFromJson(_obj, "xoffset", xoffset.get()) : 0D,
          _obj.has("yoffset") ? JsonBindings.fieldFromJson(_obj, "yoffset", yoffset.get()) : 0D,
          JsonBindings.fieldFromJson(_obj, "object", object.get())
        );
      }
    };
  }
}

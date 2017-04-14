/* Code generated from adl module picture */

package adl.picture;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
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
    Translated other = (Translated) other0;
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

  /* Factory for construction of generic values */

  public static <T> Factory<Translated<T>> factory(Factory<T> factoryT) {
    return new Factory<Translated<T>>() {
      final Lazy<Factory<Double>> xoffset = new Lazy<>(() -> Factories.DOUBLE);
      final Lazy<Factory<Double>> yoffset = new Lazy<>(() -> Factories.DOUBLE);
      final Lazy<Factory<T>> object = new Lazy<>(() -> factoryT);

      public Translated<T> create() {
        return new Translated<T>(
          0,
          0,
          object.get().create()
          );
      }

      public Translated<T> create(Translated<T> other) {
        return new Translated<T>(
          other.getXoffset(),
          other.getYoffset(),
          object.get().create(other.getObject())
          );
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
      public Factory<Translated<T>> factory() {
        return _factory;
      }

      public JsonElement toJson(Translated<T> _value) {
        JsonObject _result = new JsonObject();
        _result.add("xoffset", xoffset.get().toJson(_value.xoffset));
        _result.add("yoffset", yoffset.get().toJson(_value.yoffset));
        _result.add("object", object.get().toJson(_value.object));
        return _result;
      }

      public Translated<T> fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new Translated<T>(
          _obj.has("xoffset") ? xoffset.get().fromJson(_obj.get("xoffset")) : 0,
          _obj.has("yoffset") ? yoffset.get().fromJson(_obj.get("yoffset")) : 0,
          _obj.has("object") ? object.get().fromJson(_obj.get("object")) : factoryT.create()
        );
      }
    };
  }
}

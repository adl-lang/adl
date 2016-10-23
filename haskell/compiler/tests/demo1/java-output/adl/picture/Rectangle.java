package adl.picture;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import java.util.Objects;

public class Rectangle {

  /* Members */

  private double width;
  private double height;

  /* Constructors */

  public Rectangle(double width, double height) {
    this.width = width;
    this.height = height;
  }

  public Rectangle() {
    this.width = 0.0;
    this.height = 0.0;
  }

  public Rectangle(Rectangle other) {
    this.width = other.width;
    this.height = other.height;
  }

  /* Accessors and mutators */

  public double getWidth() {
    return width;
  }

  public void setWidth(double width) {
    this.width = width;
  }

  public double getHeight() {
    return height;
  }

  public void setHeight(double height) {
    this.height = height;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Rectangle)) {
      return false;
    }
    Rectangle other = (Rectangle) other0;
    return
      width == other.width &&
      height == other.height;
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + Double.valueOf(width).hashCode();
    result = result * 37 + Double.valueOf(height).hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<Rectangle> FACTORY = new Factory<Rectangle>() {
    public Rectangle create() {
      return new Rectangle();
    }
    public Rectangle create(Rectangle other) {
      return new Rectangle(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<Rectangle> jsonBinding() {
    final Lazy<JsonBinding<Double>> width = new Lazy<>(() -> JsonBindings.DOUBLE);
    final Lazy<JsonBinding<Double>> height = new Lazy<>(() -> JsonBindings.DOUBLE);
    final Factory<Rectangle> _factory = FACTORY;

    return new JsonBinding<Rectangle>() {
      public Factory<Rectangle> factory() {
        return _factory;
      }

      public JsonElement toJson(Rectangle _value) {
        JsonObject _result = new JsonObject();
        _result.add("width", width.get().toJson(_value.width));
        _result.add("height", height.get().toJson(_value.height));
        return _result;
      }

      public Rectangle fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new Rectangle(
          _obj.has("width") ? width.get().fromJson(_obj.get("width")) : 0.0,
          _obj.has("height") ? height.get().fromJson(_obj.get("height")) : 0.0
        );
      }
    };
  }
}

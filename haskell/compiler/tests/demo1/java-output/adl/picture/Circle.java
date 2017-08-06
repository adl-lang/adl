/* Code generated from adl module picture */

package adl.picture;

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

public class Circle {

  /* Members */

  private double radius;

  /* Constructors */

  public Circle(double radius) {
    this.radius = radius;
  }

  public Circle() {
    this.radius = 0.0;
  }

  public Circle(Circle other) {
    this.radius = other.radius;
  }

  /* Accessors and mutators */

  public double getRadius() {
    return radius;
  }

  public void setRadius(double radius) {
    this.radius = radius;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Circle)) {
      return false;
    }
    Circle other = (Circle) other0;
    return
      radius == other.radius;
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + Double.valueOf(radius).hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<Circle> FACTORY = new Factory<Circle>() {
    @Override
    public Circle create() {
      return new Circle();
    }

    @Override
    public Circle create(Circle other) {
      return new Circle(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("picture", "Circle");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
  };

  /* Json serialization */

  public static JsonBinding<Circle> jsonBinding() {
    final Lazy<JsonBinding<Double>> radius = new Lazy<>(() -> JsonBindings.DOUBLE);
    final Factory<Circle> _factory = FACTORY;

    return new JsonBinding<Circle>() {
      @Override
      public Factory<Circle> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Circle _value) {
        JsonObject _result = new JsonObject();
        _result.add("radius", radius.get().toJson(_value.radius));
        return _result;
      }

      @Override
      public Circle fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new Circle(
          JsonBindings.fieldFromJson(_obj, "radius", radius.get())
        );
      }
    };
  }
}

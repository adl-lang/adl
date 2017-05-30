/* Code generated from adl module picture */

package adl.picture;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.JsonParseException;
import org.adl.runtime.Lazy;
import java.util.ArrayList;
import java.util.Map;
import java.util.Objects;

public class Picture {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The Picture discriminator type.
   */
  public enum Disc {
    CIRCLE,
    RECTANGLE,
    COMPOSED,
    TRANSLATED
  }

  /* Constructors */

  public static Picture circle(Circle v) {
    return new Picture(Disc.CIRCLE, Objects.requireNonNull(v));
  }

  public static Picture rectangle(Rectangle v) {
    return new Picture(Disc.RECTANGLE, Objects.requireNonNull(v));
  }

  public static Picture composed(ArrayList<Picture> v) {
    return new Picture(Disc.COMPOSED, Objects.requireNonNull(v));
  }

  public static Picture translated(Translated<Picture> v) {
    return new Picture(Disc.TRANSLATED, Objects.requireNonNull(v));
  }

  public Picture() {
    this.disc = Disc.CIRCLE;
    this.value = new Circle();
  }

  public Picture(Picture other) {
    this.disc = other.disc;
    switch (other.disc) {
      case CIRCLE:
        this.value = Circle.FACTORY.create((Circle) other.value);
        break;
      case RECTANGLE:
        this.value = Rectangle.FACTORY.create((Rectangle) other.value);
        break;
      case COMPOSED:
        this.value = Factories.arrayList(Picture.FACTORY).create(Picture.<ArrayList<Picture>>cast(other.value));
        break;
      case TRANSLATED:
        this.value = Translated.factory(Picture.FACTORY).create(Picture.<Translated<Picture>>cast(other.value));
        break;
    }
  }

  private Picture(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public Circle getCircle() {
    if (disc == Disc.CIRCLE) {
      return (Circle) value;
    }
    throw new IllegalStateException();
  }

  public Rectangle getRectangle() {
    if (disc == Disc.RECTANGLE) {
      return (Rectangle) value;
    }
    throw new IllegalStateException();
  }

  public ArrayList<Picture> getComposed() {
    if (disc == Disc.COMPOSED) {
      return Picture.<ArrayList<Picture>>cast(value);
    }
    throw new IllegalStateException();
  }

  public Translated<Picture> getTranslated() {
    if (disc == Disc.TRANSLATED) {
      return Picture.<Translated<Picture>>cast(value);
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setCircle(Circle v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.CIRCLE;
  }

  public void setRectangle(Rectangle v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.RECTANGLE;
  }

  public void setComposed(ArrayList<Picture> v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.COMPOSED;
  }

  public void setTranslated(Translated<Picture> v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.TRANSLATED;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Picture)) {
      return false;
    }
    Picture other = (Picture) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  @SuppressWarnings("unchecked")
  private static <T> T cast(final Object o) {
    return (T) o;
  }

  /* Factory for construction of generic values */

  public static final Factory<Picture> FACTORY = new Factory<Picture>() {
    public Picture create() {
      return new Picture();
    }
    public Picture create(Picture other) {
      return new Picture(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<Picture> jsonBinding() {
    final Lazy<JsonBinding<Circle>> circle = new Lazy<>(() -> Circle.jsonBinding());
    final Lazy<JsonBinding<Rectangle>> rectangle = new Lazy<>(() -> Rectangle.jsonBinding());
    final Lazy<JsonBinding<ArrayList<Picture>>> composed = new Lazy<>(() -> JsonBindings.arrayList(adl.picture.Picture.jsonBinding()));
    final Lazy<JsonBinding<Translated<Picture>>> translated = new Lazy<>(() -> Translated.jsonBinding(adl.picture.Picture.jsonBinding()));
    final Factory<Picture> _factory = FACTORY;

    return new JsonBinding<Picture>() {
      public Factory<Picture> factory() {
        return _factory;
      }

      public JsonElement toJson(Picture _value) {
        switch (_value.getDisc()) {
          case CIRCLE:
            return JsonBindings.unionToJson("circle", _value.getCircle(), circle.get());
          case RECTANGLE:
            return JsonBindings.unionToJson("rectangle", _value.getRectangle(), rectangle.get());
          case COMPOSED:
            return JsonBindings.unionToJson("composed", _value.getComposed(), composed.get());
          case TRANSLATED:
            return JsonBindings.unionToJson("translated", _value.getTranslated(), translated.get());
        }
        return null;
      }

      public Picture fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("circle")) {
          return Picture.circle(JsonBindings.unionValueFromJson(_json, circle.get()));
        }
        else if (_key.equals("rectangle")) {
          return Picture.rectangle(JsonBindings.unionValueFromJson(_json, rectangle.get()));
        }
        else if (_key.equals("composed")) {
          return Picture.composed(JsonBindings.unionValueFromJson(_json, composed.get()));
        }
        else if (_key.equals("translated")) {
          return Picture.translated(JsonBindings.unionValueFromJson(_json, translated.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union Picture");
      }
    };
  }
}

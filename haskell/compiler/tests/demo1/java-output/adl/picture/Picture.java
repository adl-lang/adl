package adl.picture;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
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
        JsonObject _result = new JsonObject();
        switch (_value.getDisc()) {
          case CIRCLE:
            _result.add("circle", circle.get().toJson(_value.getCircle()));
            break;
          case RECTANGLE:
            _result.add("rectangle", rectangle.get().toJson(_value.getRectangle()));
            break;
          case COMPOSED:
            _result.add("composed", composed.get().toJson(_value.getComposed()));
            break;
          case TRANSLATED:
            _result.add("translated", translated.get().toJson(_value.getTranslated()));
            break;
        }
        return _result;
      }

      public Picture fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        for (Map.Entry<String,JsonElement> _v : _obj.entrySet()) {
          if (_v.getKey().equals("circle")) {
            return Picture.circle(circle.get().fromJson(_v.getValue()));
          }
          else if (_v.getKey().equals("rectangle")) {
            return Picture.rectangle(rectangle.get().fromJson(_v.getValue()));
          }
          else if (_v.getKey().equals("composed")) {
            return Picture.composed(composed.get().fromJson(_v.getValue()));
          }
          else if (_v.getKey().equals("translated")) {
            return Picture.translated(translated.get().fromJson(_v.getValue()));
          }
        }
        throw new IllegalStateException();
      }
    };
  }
}

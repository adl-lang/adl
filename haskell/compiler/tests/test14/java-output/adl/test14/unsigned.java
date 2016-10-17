package adl.test14;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import java.util.Map;
import java.util.Objects;

public class unsigned {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The unsigned discriminator type.
   */
  public enum Disc {
    NULL_
  }

  /* Constructors */

  public static unsigned null_() {
    return new unsigned(Disc.NULL_, null);
  }

  public unsigned() {
    this.disc = Disc.NULL_;
    this.value = null;
  }

  public unsigned(unsigned other) {
    this.disc = other.disc;
    switch (other.disc) {
      case NULL_:
        this.value = (Void) other.value;
        break;
    }
  }

  private unsigned(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  /* Mutators */

  public void setNull() {
    this.value = null;
    this.disc = Disc.NULL_;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof unsigned)) {
      return false;
    }
    unsigned other = (unsigned) other0;
    return disc == other.disc;
  }

  @Override
  public int hashCode() {
    return disc.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<unsigned> FACTORY = new Factory<unsigned>() {
    public unsigned create() {
      return new unsigned();
    }
    public unsigned create(unsigned other) {
      return new unsigned(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<unsigned> jsonBinding() {
    final Lazy<JsonBinding<Void>> null_ = new Lazy<>(() -> JsonBindings.VOID);
    final Factory<unsigned> _factory = FACTORY;

    return new JsonBinding<unsigned>() {
      public Factory<unsigned> factory() {
        return _factory;
      }

      public JsonElement toJson(unsigned _value) {
        JsonObject _result = new JsonObject();
        switch (_value.getDisc()) {
          case NULL_:
            _result.add("null", null);
        }
        return _result;
      }

      public unsigned fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        for (Map.Entry<String,JsonElement> _v : _obj.entrySet()) {
          if (_v.getKey().equals("null")) {
            return unsigned.null_();
          }
        }
        throw new IllegalStateException();
      }
    };
  }
}

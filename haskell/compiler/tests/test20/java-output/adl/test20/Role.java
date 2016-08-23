package adl.test20;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import java.util.Map;
import java.util.Objects;

public class Role {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The Role discriminator type.
   */
  public enum Disc {
    UNDERLING,
    BOSS,
    SUPERBOSS
  }

  /* Constructors */

  public static Role underling() {
    return new Role(Disc.UNDERLING, null);
  }

  public static Role boss() {
    return new Role(Disc.BOSS, null);
  }

  public static Role superBoss() {
    return new Role(Disc.SUPERBOSS, null);
  }

  public Role() {
    this.disc = Disc.UNDERLING;
    this.value = null;
  }

  public Role(Role other) {
    this.disc = other.disc;
    switch (other.disc) {
      case UNDERLING:
        this.value = (Void) other.value;
        break;
      case BOSS:
        this.value = (Void) other.value;
        break;
      case SUPERBOSS:
        this.value = (Void) other.value;
        break;
    }
  }

  private Role(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  /* Mutators */

  public void setUnderling() {
    this.value = null;
    this.disc = Disc.UNDERLING;
  }

  public void setBoss() {
    this.value = null;
    this.disc = Disc.BOSS;
  }

  public void setSuperBoss() {
    this.value = null;
    this.disc = Disc.SUPERBOSS;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Role)) {
      return false;
    }
    Role other = (Role) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<Role> FACTORY = new Factory<Role>() {
    public Role create() {
      return new Role();
    }
    public Role create(Role other) {
      return new Role(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<Role> jsonBinding() {
    final JsonBinding<Void> underling = JsonBindings.VOID;
    final JsonBinding<Void> boss = JsonBindings.VOID;
    final JsonBinding<Void> superBoss = JsonBindings.VOID;
    final Factory<Role> _factory = FACTORY;

    return new JsonBinding<Role>() {
      public Factory<Role> factory() {
        return _factory;
      }

      public JsonElement toJson(Role _value) {
        JsonObject _result = new JsonObject();
        switch (_value.getDisc()) {
          case UNDERLING:
            _result.add("u", null);
          case BOSS:
            _result.add("b", null);
          case SUPERBOSS:
            _result.add("sb", null);
        }
        return _result;
      }

      public Role fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        for (Map.Entry<String,JsonElement> _v : _obj.entrySet()) {
          if (_v.getKey() == "u") {
            return Role.underling();
          }
          else if (_v.getKey() == "b") {
            return Role.boss();
          }
          else if (_v.getKey() == "sb") {
            return Role.superBoss();
          }
        }
        throw new IllegalStateException();
      }
    };
  }
}

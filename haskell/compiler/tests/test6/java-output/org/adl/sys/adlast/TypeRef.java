package org.adl.sys.adlast;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import java.util.Map;
import java.util.Objects;

public class TypeRef {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The TypeRef discriminator type.
   */
  public enum Disc {
    PRIMITIVE,
    TYPEPARAM,
    REFERENCE
  }

  /* Constructors */

  public static TypeRef primitive(String v) {
    return new TypeRef(Disc.PRIMITIVE, Objects.requireNonNull(v));
  }

  public static TypeRef typeParam(String v) {
    return new TypeRef(Disc.TYPEPARAM, Objects.requireNonNull(v));
  }

  public static TypeRef reference(ScopedName v) {
    return new TypeRef(Disc.REFERENCE, Objects.requireNonNull(v));
  }

  public TypeRef() {
    this.disc = Disc.PRIMITIVE;
    this.value = "";
  }

  public TypeRef(TypeRef other) {
    this.disc = other.disc;
    switch (other.disc) {
      case PRIMITIVE:
        this.value = (String) other.value;
        break;
      case TYPEPARAM:
        this.value = (String) other.value;
        break;
      case REFERENCE:
        this.value = ScopedName.FACTORY.create((ScopedName) other.value);
        break;
    }
  }

  private TypeRef(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public String getPrimitive() {
    if (disc == Disc.PRIMITIVE) {
      return (String) value;
    }
    throw new IllegalStateException();
  }

  public String getTypeParam() {
    if (disc == Disc.TYPEPARAM) {
      return (String) value;
    }
    throw new IllegalStateException();
  }

  public ScopedName getReference() {
    if (disc == Disc.REFERENCE) {
      return (ScopedName) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setPrimitive(String v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.PRIMITIVE;
  }

  public void setTypeParam(String v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.TYPEPARAM;
  }

  public void setReference(ScopedName v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.REFERENCE;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof TypeRef)) {
      return false;
    }
    TypeRef other = (TypeRef) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<TypeRef> FACTORY = new Factory<TypeRef>() {
    public TypeRef create() {
      return new TypeRef();
    }
    public TypeRef create(TypeRef other) {
      return new TypeRef(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<TypeRef> jsonBinding() {
    final JsonBinding<String> primitive = JsonBindings.STRING;
    final JsonBinding<String> typeParam = JsonBindings.STRING;
    final JsonBinding<ScopedName> reference = ScopedName.jsonBinding();
    final Factory<TypeRef> _factory = FACTORY;

    return new JsonBinding<TypeRef>() {
      public Factory<TypeRef> factory() {
        return _factory;
      }

      public JsonElement toJson(TypeRef _value) {
        JsonObject _result = new JsonObject();
        switch (_value.getDisc()) {
          case PRIMITIVE:
            _result.add("primitive", primitive.toJson(_value.getPrimitive()));
            break;
          case TYPEPARAM:
            _result.add("typeParam", typeParam.toJson(_value.getTypeParam()));
            break;
          case REFERENCE:
            _result.add("reference", reference.toJson(_value.getReference()));
            break;
        }
        return _result;
      }

      public TypeRef fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        for (Map.Entry<String,JsonElement> _v : _obj.entrySet()) {
          if (_v.getKey().equals("primitive")) {
            return TypeRef.primitive(primitive.fromJson(_v.getValue()));
          }
          else if (_v.getKey().equals("typeParam")) {
            return TypeRef.typeParam(typeParam.fromJson(_v.getValue()));
          }
          else if (_v.getKey().equals("reference")) {
            return TypeRef.reference(reference.fromJson(_v.getValue()));
          }
        }
        throw new IllegalStateException();
      }
    };
  }
}

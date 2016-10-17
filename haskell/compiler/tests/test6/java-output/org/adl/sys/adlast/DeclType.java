package org.adl.sys.adlast;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.Lazy;
import java.util.Map;
import java.util.Objects;

public class DeclType {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The DeclType discriminator type.
   */
  public enum Disc {
    STRUCT_,
    UNION_,
    TYPE_,
    NEWTYPE_
  }

  /* Constructors */

  public static DeclType struct_(Struct v) {
    return new DeclType(Disc.STRUCT_, Objects.requireNonNull(v));
  }

  public static DeclType union_(Union v) {
    return new DeclType(Disc.UNION_, Objects.requireNonNull(v));
  }

  public static DeclType type_(TypeDef v) {
    return new DeclType(Disc.TYPE_, Objects.requireNonNull(v));
  }

  public static DeclType newtype_(NewType v) {
    return new DeclType(Disc.NEWTYPE_, Objects.requireNonNull(v));
  }

  public DeclType() {
    this.disc = Disc.STRUCT_;
    this.value = new Struct();
  }

  public DeclType(DeclType other) {
    this.disc = other.disc;
    switch (other.disc) {
      case STRUCT_:
        this.value = Struct.FACTORY.create((Struct) other.value);
        break;
      case UNION_:
        this.value = Union.FACTORY.create((Union) other.value);
        break;
      case TYPE_:
        this.value = TypeDef.FACTORY.create((TypeDef) other.value);
        break;
      case NEWTYPE_:
        this.value = NewType.FACTORY.create((NewType) other.value);
        break;
    }
  }

  private DeclType(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public Struct getStruct_() {
    if (disc == Disc.STRUCT_) {
      return (Struct) value;
    }
    throw new IllegalStateException();
  }

  public Union getUnion_() {
    if (disc == Disc.UNION_) {
      return (Union) value;
    }
    throw new IllegalStateException();
  }

  public TypeDef getType_() {
    if (disc == Disc.TYPE_) {
      return (TypeDef) value;
    }
    throw new IllegalStateException();
  }

  public NewType getNewtype_() {
    if (disc == Disc.NEWTYPE_) {
      return (NewType) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setStruct_(Struct v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.STRUCT_;
  }

  public void setUnion_(Union v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.UNION_;
  }

  public void setType_(TypeDef v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.TYPE_;
  }

  public void setNewtype_(NewType v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.NEWTYPE_;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof DeclType)) {
      return false;
    }
    DeclType other = (DeclType) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  /* Factory for construction of generic values */

  public static final Factory<DeclType> FACTORY = new Factory<DeclType>() {
    public DeclType create() {
      return new DeclType();
    }
    public DeclType create(DeclType other) {
      return new DeclType(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<DeclType> jsonBinding() {
    final Lazy<JsonBinding<Struct>> struct_ = new Lazy<>(() -> Struct.jsonBinding());
    final Lazy<JsonBinding<Union>> union_ = new Lazy<>(() -> Union.jsonBinding());
    final Lazy<JsonBinding<TypeDef>> type_ = new Lazy<>(() -> TypeDef.jsonBinding());
    final Lazy<JsonBinding<NewType>> newtype_ = new Lazy<>(() -> NewType.jsonBinding());
    final Factory<DeclType> _factory = FACTORY;

    return new JsonBinding<DeclType>() {
      public Factory<DeclType> factory() {
        return _factory;
      }

      public JsonElement toJson(DeclType _value) {
        JsonObject _result = new JsonObject();
        switch (_value.getDisc()) {
          case STRUCT_:
            _result.add("struct_", struct_.get().toJson(_value.getStruct_()));
            break;
          case UNION_:
            _result.add("union_", union_.get().toJson(_value.getUnion_()));
            break;
          case TYPE_:
            _result.add("type_", type_.get().toJson(_value.getType_()));
            break;
          case NEWTYPE_:
            _result.add("newtype_", newtype_.get().toJson(_value.getNewtype_()));
            break;
        }
        return _result;
      }

      public DeclType fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        for (Map.Entry<String,JsonElement> _v : _obj.entrySet()) {
          if (_v.getKey().equals("struct_")) {
            return DeclType.struct_(struct_.get().fromJson(_v.getValue()));
          }
          else if (_v.getKey().equals("union_")) {
            return DeclType.union_(union_.get().fromJson(_v.getValue()));
          }
          else if (_v.getKey().equals("type_")) {
            return DeclType.type_(type_.get().fromJson(_v.getValue()));
          }
          else if (_v.getKey().equals("newtype_")) {
            return DeclType.newtype_(newtype_.get().fromJson(_v.getValue()));
          }
        }
        throw new IllegalStateException();
      }
    };
  }
}

package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import java.util.Objects;

/**
 * A structure containing primitives.
 * 
 * It has two fields: an integer x and a String y.
 */
public class S1 {

  /* Members */

  private int x;
  private String y;

  /* Constructors */

  public S1(int x, String y) {
    this.x = x;
    this.y = Objects.requireNonNull(y);
  }

  public S1() {
    this.x = 0;
    this.y = "";
  }

  public S1(S1 other) {
    this.x = other.x;
    this.y = other.y;
  }

  /* Accessors and mutators */

  public int getX() {
    return x;
  }

  public void setX(int newX) {
    x = newX;
  }

  public String getY() {
    return y;
  }

  public void setY(String newY) {
    y = Objects.requireNonNull(newY);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S1)) {
      return false;
    }
    S1 other = (S1) other0;
    return
      x == other.x &&
      y.equals(other.y);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + x;
    result = result * 37 + y.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<S1> FACTORY = new Factory<S1>() {
    public S1 create() {
      return new S1();
    }
    public S1 create(S1 other) {
      return new S1(other);
    }
  };
}

package adl.test;

public class S1 {

  public int x;
  public String y;

  public S1(int x, String y) {
    this.x = x;
    this.y = y;
  }

  public S1() {
    this.x = 0;
    this.y = "";
  }

  public S1(S1 other) {
    this.x = other.x;
    this.y = other.y;
  }
}
package adl.test;

public class S2 {

  public String f1;
  public double f2;
  public java.util.ArrayList<Int> f3;

  public S2(String f1, double f2, java.util.ArrayList<Int> f3) {
    this.f1 = f1;
    this.f2 = f2;
    this.f3 = f3;
  }

  public S2() {
    this.f1 = "";
    this.f2 = 0;
    this.f3 = new java.util.ArrayList<Int>();
  }

  public S2(S2 other) {
    this.f1 = other.f1;
    this.f2 = other.f2;
    this.f3 = new java.util.ArrayList<Int>(other.f3);
  }
}
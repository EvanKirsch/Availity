public class BenefitsIncomingDTO implements Comparable<BenefitsIncomingDTO> {
  private String userId;
  private String firstName;
  private String lastName;
  private Integer version;
  private String InsuranceCompany;

  public String getUserId() {
    return userId;
  }

  public void setUserId(String userId) {
    this.userId = userId;
  }

  public String getFirstName() {
    return firstName;
  }

  public void setFirstName(String firstName) {
    this.firstName = firstName;
  }

  public String getLastName() {
    return lastName;
  }

  public void setLastName(String lastName) {
    this.lastName = lastName;
  }

  public Integer getVersion() {
    return version;
  }

  public void setVersion(Integer version) {
    this.version = version;
  }

  public String getInsuranceCompany() {
    return InsuranceCompany;
  }

  public void setInsuranceCompany(String insuranceCompany) {
    InsuranceCompany = insuranceCompany;
  }

  @Override
  public String toString() {
    return "BenefitsIncomingDTO{" +
        "userId='" + userId + '\'' +
        ", firstName='" + firstName + '\'' +
        ", lastName='" + lastName + '\'' +
        ", version=" + version +
        ", InsuranceCompany='" + InsuranceCompany + '\'' +
        '}';
  }

  @Override
  public int compareTo(BenefitsIncomingDTO that) {
    String thisFullName = this.getLastName() + this.getFirstName();
    String thatFullName = that.getLastName() + that.getFirstName();
    return thisFullName.compareTo(thatFullName);
  }
}

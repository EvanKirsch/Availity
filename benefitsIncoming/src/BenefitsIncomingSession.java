import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;

public class BenefitsIncomingSession {

  public static final String OUT_FILE_PATH = "benefitsIncoming/test/";

  public static final int INDEX_USER_ID = 0;
  public static final int INDEX_FIRST_NAME = 1;
  public static final int INDEX_LAST_NAME = 2;
  public static final int INDEX_VERSION = 3;
  public static final int INDEX_INSURANCE_COMPANY = 4;

  public static void main(String[] args) {
    List<List<String>> records = readFile(args[0]);
    List<BenefitsIncomingDTO> incoming = marshalRecords(records);

    Map<String, List<BenefitsIncomingDTO>> files = splitFiles(incoming);
    files.forEach((k, v) -> filterVersions(v));
    files.forEach((k, v) -> v.sort(BenefitsIncomingDTO::compareTo));
    files.forEach(BenefitsIncomingSession::printFile);

  }

  private static void printFile(String company, List<BenefitsIncomingDTO> benefits) {
    File file = new File(OUT_FILE_PATH + company + ".out");
    try {
      FileWriter outfile = new FileWriter(file);
      for (BenefitsIncomingDTO benefit : benefits) {
        writeRecord(benefit, outfile);
      }
      outfile.close();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  private static void writeRecord(BenefitsIncomingDTO benefit, FileWriter writer)
      throws IOException {
    writer.write(benefit.getUserId() + ",");
    writer.write(benefit.getFirstName() + ",");
    writer.write(benefit.getLastName() + ",");
    writer.write(benefit.getVersion() + ",");
    writer.write(benefit.getInsuranceCompany() + "\n");
  }


  private static void filterVersions(List<BenefitsIncomingDTO> benefits) {

    // Orders highest to lowest by version number
    benefits.sort(Comparator.comparing(BenefitsIncomingDTO::getVersion));
    Collections.reverse(benefits);

    Set<String> versions = new HashSet<>();
    for (BenefitsIncomingDTO benefit : benefits) {
      if (!versions.contains(benefit.getUserId())) {
        versions.add(benefit.getUserId());
      } else {
        benefits.remove(benefit);
      }
    }
  }

  private static Map<String, List<BenefitsIncomingDTO>> splitFiles(List<BenefitsIncomingDTO> benefits) {
    Map<String, List<BenefitsIncomingDTO>> files = new HashMap<>();
    for (BenefitsIncomingDTO benefit : benefits) {
      String insCompany = benefit.getInsuranceCompany();
      List<BenefitsIncomingDTO> insCompanyBenefits =
          files.get(insCompany) == null ? new ArrayList<>() : files.get(insCompany);
      insCompanyBenefits.add(benefit);
      files.put(insCompany, insCompanyBenefits);
    }
    return files;
  }

  private static List<BenefitsIncomingDTO> marshalRecords(List<List<String>> records) {
    List<BenefitsIncomingDTO> incoming = new ArrayList<>();
    for (List<String> record : records) {
      BenefitsIncomingDTO benefit = new BenefitsIncomingDTO();
      try {
        benefit.setUserId(record.get(INDEX_USER_ID).trim());
        benefit.setFirstName(record.get(INDEX_FIRST_NAME).trim());
        benefit.setLastName(record.get(INDEX_LAST_NAME).trim());
        benefit.setVersion(Integer.parseInt(record.get(INDEX_VERSION).trim()));
        benefit.setInsuranceCompany(record.get(INDEX_INSURANCE_COMPANY).trim());
        incoming.add(benefit);
      } catch (Exception e) {
        // Exception handling out of scope
        System.out.println("Warning Bad Record: " + record);
      }
    }
    return incoming;
  }

  private static List<List<String>> readFile(String path) {
    File file = new File(path);
    Scanner scanner;
    try {
      scanner = new Scanner(file);
    } catch (FileNotFoundException e) {
      e.printStackTrace();
      return new ArrayList<>();
    }

    List<List<String>> records = new ArrayList<>();
    while (scanner.hasNext()) {
      List<String> record = Arrays.asList(scanner.nextLine().split(","));
      records.add(record);
    }
    return records;
  }

}

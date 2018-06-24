package net.digihippo.xenoquiz;

import javax.net.ssl.*;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;

@WebServlet(name = "XenoCantoProxy", value = "/api/birds")
public class XenoCantoProxy extends HttpServlet {

  @Override
  public void doGet(HttpServletRequest request, HttpServletResponse response)
      throws IOException {

    final String birdName = URLEncoder.encode(
        request.getParameter("birdName"), StandardCharsets.UTF_8.name());
    final String xenoApiUrl = "https://www.xeno-canto.org/api/2/recordings?query=" + birdName;

    final URL url = new URL(xenoApiUrl);
    final HttpURLConnection connection = (HttpURLConnection) url.openConnection();

    if (connection.getResponseCode() < 300)
    {
      response.setStatus(200);
      response.setHeader("Content-Type", "application/json");
      final byte[] buffer = new byte[1024];
      try (
          final InputStream inputStream = connection.getInputStream();
          final OutputStream outputStream = response.getOutputStream()
      )
      {
        int readCount = inputStream.read(buffer);
        while (readCount != -1)
        {
          outputStream.write(buffer, 0, readCount);

          readCount = inputStream.read(buffer);
        }
      }
    }
    else
    {
      response.setStatus(connection.getResponseCode());
    }
  }
}

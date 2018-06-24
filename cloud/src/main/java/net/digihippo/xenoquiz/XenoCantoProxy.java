package net.digihippo.xenoquiz;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@WebServlet(name = "XenoCantoProxy", value = "/api/birds")
public class XenoCantoProxy extends HttpServlet {

  private static final Map<String, String> cache = new ConcurrentHashMap<>();

  @Override
  public void doGet(HttpServletRequest request, HttpServletResponse response)
      throws IOException {

    final String birdNameRaw = request.getParameter("birdName");
    final String encodedName = URLEncoder.encode(
        birdNameRaw, StandardCharsets.UTF_8.name());

    if (cache.get(birdNameRaw) == null)
    {
      final String xenoApiUrl = "https://www.xeno-canto.org/api/2/recordings?query=" + encodedName;
      final URL url = new URL(xenoApiUrl);
      final HttpURLConnection connection = (HttpURLConnection) url.openConnection();

      if (connection.getResponseCode() < 300)
      {
        final StringBuilder builder = new StringBuilder();
        final byte[] buffer = new byte[1024];
        try (final InputStream inputStream = connection.getInputStream())
        {
          int readCount = inputStream.read(buffer);
          while (readCount != -1)
          {
            builder.append(new String(buffer, 0, readCount, StandardCharsets.UTF_8));

            readCount = inputStream.read(buffer);
          }

          cache.put(birdNameRaw, builder.toString());
        }
      }
      else
      {
        response.setStatus(connection.getResponseCode());
      }
    }

    response.setStatus(200);
    response.setHeader("Content-Type", "application/json");
    response.getOutputStream().print(cache.get(birdNameRaw));
    response.getOutputStream().close();
  }
}

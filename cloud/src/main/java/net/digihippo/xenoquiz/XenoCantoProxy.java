package net.digihippo.xenoquiz;

import com.google.gson.Gson;
import com.google.gson.stream.JsonReader;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

@WebServlet(name = "XenoCantoProxy", value = "/api/birds")
public class XenoCantoProxy extends HttpServlet {

  private static final Gson gson = new Gson();
  private static final Map<String, String> cache = new ConcurrentHashMap<>();

  public static final class Recordings
  {
    private List<Recording> recordings;
  }

  public static final class Recording
  {
    private String file;
  }

  @Override
  public void doGet(HttpServletRequest request, HttpServletResponse response)
      throws IOException
  {
    final String birdNameRaw = request.getParameter("birdName");
    if (cache.get(birdNameRaw) == null)
    {
      populateCache(response, birdNameRaw);
    }

    response.setStatus(200);
    response.setHeader("Content-Type", "application/json");
    response.getOutputStream().print(cache.get(birdNameRaw));
  }

  private void populateCache(HttpServletResponse response, String birdNameRaw) throws IOException
  {
    final String encodedName = URLEncoder.encode(
        birdNameRaw, StandardCharsets.UTF_8.name());
    final String xenoApiUrl = "https://www.xeno-canto.org/api/2/recordings?query=" + encodedName;
    final URL url = new URL(xenoApiUrl);
    final HttpURLConnection connection = (HttpURLConnection) url.openConnection();

    if (connection.getResponseCode() < 300)
    {
      try (final InputStream inputStream = connection.getInputStream())
      {
        final Recordings recordings =
            gson.fromJson(new JsonReader(new InputStreamReader(inputStream)), Recordings.class);


        final List<Recording> trimmer = recordings.recordings
            .stream()
            .map(r -> r.file.replace("//www.xeno-canto.org/", ""))
            .map(u -> {
              Recording r = new Recording();
              r.file = u;
              return r;
            })
            .collect(Collectors.toList());

        Recordings trimmed = new Recordings();
        trimmed.recordings = trimmer;
        cache.put(birdNameRaw, gson.toJson(trimmed));
      }
    }
    else
    {
      response.setStatus(connection.getResponseCode());
    }
  }
}

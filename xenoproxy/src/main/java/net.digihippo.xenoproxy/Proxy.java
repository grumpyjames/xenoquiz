package net.digihippo.xenoproxy;

import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.handler.AbstractHandler;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.*;
import java.nio.charset.StandardCharsets;

class Proxy
{
    public static void main(String[] args) throws Exception {
        Server server = new Server(8080);
        server.setHandler(new AbstractHandler() {
            @Override
            public void handle(
                    String target,
                    Request baseRequest,
                    HttpServletRequest request,
                    HttpServletResponse response) throws IOException {
                // Should already be url encoded, so just pass it through.
                final String birdName = URLEncoder.encode(
                        baseRequest.getParameter("birdName"), StandardCharsets.UTF_8.name());
                final String xenoApiUrl = "https://www.xeno-canto.org/api/2/recordings?query=" + birdName;

                System.out.println(xenoApiUrl);

                final URL url = new URL(xenoApiUrl);
                final HttpURLConnection connection = (HttpURLConnection) url.openConnection();

                if (connection.getResponseCode() < 300)
                {
                    response.setStatus(200);
                    response.setHeader("Content-Type", "application/json");
                    response.setHeader("Access-Control-Allow-Origin", "*");
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
                    System.out.println(connection.getResponseCode());
                    response.setStatus(connection.getResponseCode());
                }


            }
        });

        server.start();
        server.join();
    }
}
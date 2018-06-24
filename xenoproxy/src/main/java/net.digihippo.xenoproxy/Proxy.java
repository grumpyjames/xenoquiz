package net.digihippo.xenoproxy;

import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.handler.AbstractHandler;

import javax.net.ssl.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.stream.IntStream;

class Proxy
{
    public static void main(String[] args) throws Exception {
        // Create a trust manager that does not validate certificate chains
        TrustManager[] trustAllCerts = new TrustManager[] {new X509TrustManager() {
            public java.security.cert.X509Certificate[] getAcceptedIssuers() {
                return null;
            }
            public void checkClientTrusted(X509Certificate[] certs, String authType) {
            }
            public void checkServerTrusted(X509Certificate[] certs, String authType) {
            }
        }
        };

        // Install the all-trusting trust manager
        SSLContext sc = SSLContext.getInstance("SSL");
        sc.init(null, trustAllCerts, new java.security.SecureRandom());
        HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());

        // Create all-trusting host name verifier
        HostnameVerifier allHostsValid = new HostnameVerifier() {
            public boolean verify(String hostname, SSLSession session) {
                return true;
            }
        };

        // Install the all-trusting host verifier
        HttpsURLConnection.setDefaultHostnameVerifier(allHostsValid);

        Server server = new Server(8080);
        server.setHandler(new AbstractHandler() {
            @Override
            public void handle(
                    String target,
                    Request baseRequest,
                    HttpServletRequest request,
                    HttpServletResponse response) throws IOException {
                final String birdName = URLEncoder.encode(
                        baseRequest.getParameter("birdName"), StandardCharsets.UTF_8.name());
                final String xenoApiUrl = "https://www.xeno-canto.org/api/2/recordings?query=" + birdName;

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
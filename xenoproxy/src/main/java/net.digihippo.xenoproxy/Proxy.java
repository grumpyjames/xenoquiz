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
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.IntStream;

class Proxy
{
    public static void main(String[] args) throws Exception {

        System.out.println(Arrays.toString(permutation(2, new int[]{4, 5, 6, 7}, new int[0])));

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

    private static int factorial(final int num) {
        return IntStream.rangeClosed(2, num).reduce(1, (x, y) -> x * y);
    }

    private static int[] permutation(final int count, final int[] input, final int[] output) {
        if (input.length == 0) { return output; }

        final int factorial = factorial(input.length - 1);

        final int[] newOutput = new int[output.length + 1];
        System.arraycopy(output, 0, newOutput, 0, output.length);
        int itemIndex = count / factorial;
        newOutput[output.length] = input[itemIndex];

        final int[] newInput = new int[input.length - 1];
        if (itemIndex > 0)
        {
            System.arraycopy(input, 0, newInput, 0, itemIndex);
        }
        if (itemIndex != input.length - 1)
        {
            System.arraycopy(input, itemIndex + 1, newInput, itemIndex, input.length - 1 - itemIndex);
        }

        return permutation(count % factorial, newInput, newOutput);
    }
}
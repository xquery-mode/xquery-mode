
<!-- default values with suppressed parameters -->
<test-group id="group015">
  <options xmlns="http://marklogic.com/appservices/rest">
    <!-- In the rewriter, the first request will match -->
    <request uri="^/(tests/path.*)$" endpoint="/tests/endpoint.xqy?format=foo" user-params="ignore">
      <uri-param name="uri">/$1</uri-param>
      <param name="bar" default="baz"/>
    </request>
    <!-- But in the endpoint, we have to use the second request in order for
         the format parameter to go through. -->
    <request uri="^/(tests/path.*)$" endpoint="/tests/endpoint.xqy?format=foo" user-params="ignore">
      <uri-param name="uri">/$1</uri-param>
      <param name="bar" default="baz"/>
      <param name="format"/>
    </request>
  </options>
  <request-test request="2">
    <url>/path/to/thing?bar=baz</url>
    <result>
      <entry key="uri">/tests/path/to/thing</entry>
      <entry key="format">foo</entry>
      <entry key="bar">baz</entry>
    </result>
  </request-test>
</test-group>

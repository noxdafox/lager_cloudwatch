# Lager Backend for AWS CloudWatch

Lager backend for forwarding log messages to AWS CloudWatch.

## Configuration

Configure a Lager handler like the following:

```erlang
{lager_cloudwatch_backend, [Level, LogGroupName, LogStreamName]}
```

The backend will take care of creating the Log Group and Stream if non existing. Make sure the service has the correct permissions to do so. If the `LogStreamName` is `undefined` or omitted, the backend will use the node name as Log Stream name.

The backend accepts custom message formatters and related formatting configurations in the form `{Formatter, Format}`. If omitted or `undefined` a default message format will be used.

Example:

```erlang
{lager_cloudwatch_backend, [info,
                            "ExampleLogGroup",
                            "ExampleLogStream",
                            {lager_default_formatter, [time, " ", "[", severity ,"] ", " ", message, "\n"]}]}
```

AWS CloudWatch has a fixed quota of five requests per second per each stream. To avoid being throttled, the backend batches all incoming log messages in periods of one second. The period duration can be passed as additional parameter to the handler configuration.

```erlang
{lager_cloudwatch_backend, [Level, LogGroupName, LogStreamName, LogFormatter, LogPeriodMilliseconds]}
```

Example:

```erlang
{lager_cloudwatch_backend, [info, "ExampleLogGroup", "ExampleLogStream", undefined, 3000]}
```

### AWS Configuration

The Lager backend relies on [`erlcloud`](https://github.com/erlcloud/erlcloud) APIs to interface with AWS services.

To configure AWS specific parameters, simply refer to [`erlcloud`](https://github.com/erlcloud/erlcloud) README instructions.

Build
-----

    $ rebar3 compile

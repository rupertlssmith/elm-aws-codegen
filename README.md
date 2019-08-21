# elm-aws-codegen

Amazon Web Services publish service API definitions for all AWS services. These are in JSON format
and similar to Swagger definitions although the contents are not Swagger but a specification
that is specific to AWS. They play a similar role to Swagger files in that they describe the
URLs for all the service endpoints, and the data models that can be sent to and received from
these endpoints.

This code generation takes an AWS service definition and outputs Elm code with the data models
and functions to construct HTTP requests against these services.

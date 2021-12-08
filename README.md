# ABAP Platform Code Samples for on premise systems
[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/abap-platform-code-samples-standard)](https://api.reuse.software/info/github.com/SAP-samples/abap-platform-code-samples-standard)
## Description

In this repository you will find several packages that contain the sample code of various blog posts.

### How to generate RAP BOs with custom entities  
* Blog Post: [How to redefine RDS based OData services ?](https://blogs.sap.com/2021/09/21/how-to-redefine-rds-based-odata-services)  
* [Source code](../../tree/main/src/rap_gen_cust_ent)

## Requirements

This sample code can be deployed on a SAP S/4HANA system.   
You have to create a package `TEST_ABAP` beforehand in `$TMP`. 
The code for each blog post is stored in a separate sub-package (for example `TEST_ONPREM_EXTEND_RDS_SRV`) of the super package `TEST_ONPREM`.  

## Download and Installation

You can download this code and import it into an ABAP environment system using the report ZABAPGIT. Please be sure to have a package `TEST_ONPREM` in `$TMP`created beforehand.

## Known Issues

When you try to import this repository into a ABAP environment trial systems it might happen that this import does not run smoothly if another user has already performed an import.

## How to obtain support

[Create an issue](https://github.com/SAP-samples/<repository-name>/issues) in this repository if you find a bug or have questions about the content.
 
For additional support, [ask a question in SAP Community](https://answers.sap.com/questions/ask.html).

## Contributing
If you wish to contribute code, offer fixes or improvements, please send a pull request. Due to legal reasons, contributors will be asked to accept a DCO when they create the first pull request to this project. This happens in an automated fashion during the submission process. SAP uses [the standard DCO text of the Linux Foundation](https://developercertificate.org/).

## License
Copyright (c) 2021 SAP SE or an SAP affiliate company. All rights reserved. This project is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSES/Apache-2.0.txt) file.

[![REUSE status](https://api.reuse.software/badge/github.com/SAP-samples/abap-platform-code-samples-standard)](https://api.reuse.software/info/github.com/SAP-samples/abap-platform-code-samples-standard)

# ABAP Platform Code Samples for On-premise Systems

## Description

In this repository you will find several packages that contain the sample code of various blog posts.

### How to generate RAP BOs with custom entities  
* Blog Post: [How to redefine RDS based OData services ?](https://blogs.sap.com/2021/12/08/how-to-redefine-rds-based-odata-services/)  
* [Source code](../../tree/main/src/rap_gen_cust_ent)

## Requirements

This sample code can be deployed on a SAP S/4HANA system.   
You have to create a package `TEST_ABAP_STANDARD` beforehand in software component `LOCAL` without specifing a super package. 
The code for each blog post / workshop is stored in a separate sub-package (for example `TEST_ABAP_STANDARD_MS2`) of the super package `TEST_ABAP_STANDARD`.  

## Download and Installation

You can download this code and import it into an ABAP environment system using the report ZABAPGIT. Please be sure to have a package `TEST_ABAP_STANDARD` in the software component `LOCAL` without specifing a super package created beforehand.

## Known Issues

none

## How to obtain support

[Create an issue](https://github.com/SAP-samples/<repository-name>/issues) in this repository if you find a bug or have questions about the content.
 
For additional support, [ask a question in SAP Community](https://answers.sap.com/questions/ask.html).

## Contributing
If you wish to contribute code, offer fixes or improvements, please send a pull request. Due to legal reasons, contributors will be asked to accept a DCO when they create the first pull request to this project. This happens in an automated fashion during the submission process. SAP uses [the standard DCO text of the Linux Foundation](https://developercertificate.org/).

## License
Copyright (c) 2022 SAP SE or an SAP affiliate company. All rights reserved. This project is licensed under the Apache Software License, version 2.0 except as noted otherwise in the [LICENSE](LICENSES/Apache-2.0.txt) file.

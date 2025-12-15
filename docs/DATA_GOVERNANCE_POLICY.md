# Data Governance Policy

This document establishes the data governance framework for the Biobank Dashboard, ensuring data integrity, security, and compliance with institutional and regulatory requirements.

---

## Table of Contents

1. [Purpose & Scope](#1-purpose--scope)
2. [Roles & Responsibilities](#2-roles--responsibilities)
3. [Data Classification](#3-data-classification)
4. [Access Control](#4-access-control)
5. [Data Quality Standards](#5-data-quality-standards)
6. [Audit Trail Requirements](#6-audit-trail-requirements)
7. [Data Retention](#7-data-retention)
8. [Change Management](#8-change-management)
9. [Incident Response](#9-incident-response)
10. [Compliance](#10-compliance)

---

## 1. Purpose & Scope

### 1.1 Purpose

This policy establishes standards for managing biobank data throughout its lifecycle, from collection through analysis and archival. It ensures:

- Data integrity and accuracy
- Appropriate access controls
- Regulatory compliance
- Audit traceability

### 1.2 Scope

This policy applies to:

- All biobank sample metadata
- Laboratory test results (MIC qPCR, ELISA-PE, ELISA-VSG, iELISA)
- Extraction and processing records
- Dashboard application data
- Cached and derived datasets

### 1.3 Data Types Covered

| Data Category | Examples | Sensitivity |
|---------------|----------|-------------|
| Sample Metadata | Barcode, collection date, location | Medium |
| Demographics | Age, sex, pregnancy status | High |
| Clinical Data | Screening type, health facility | High |
| Test Results | Cq values, PP%, inhibition % | Medium |
| QC Metrics | Control validation, CV values | Low |

---

## 2. Roles & Responsibilities

### 2.1 Role Definitions

#### Data Owner
- **Responsibility**: Overall accountability for data quality and governance
- **Authority**: Approve access requests, policy changes
- **Typical Role**: Principal Investigator or Study Director

#### Data Steward
- **Responsibility**: Day-to-day data quality management
- **Authority**: Validate data imports, resolve quality issues
- **Typical Role**: Data Manager or Lab Coordinator

#### Data Custodian
- **Responsibility**: Technical data management and security
- **Authority**: Manage access controls, backups, system maintenance
- **Typical Role**: IT Administrator or System Administrator

#### Data User
- **Responsibility**: Use data appropriately per training and authorization
- **Authority**: View and export data within granted permissions
- **Typical Role**: Researcher, Analyst, Lab Technician

### 2.2 RACI Matrix

| Activity | Data Owner | Data Steward | Data Custodian | Data User |
|----------|------------|--------------|----------------|-----------|
| Define data policy | A | C | C | I |
| Grant access | A | R | C | I |
| Validate data quality | A | R | C | I |
| Import new data | I | A | R | I |
| System maintenance | I | I | R | I |
| Report generation | I | C | I | R |
| Incident response | A | R | R | I |

*R = Responsible, A = Accountable, C = Consulted, I = Informed*

---

## 3. Data Classification

### 3.1 Classification Levels

#### Level 1: Public
- Aggregate statistics
- De-identified summary reports
- No restrictions on access

#### Level 2: Internal
- Sample counts and metrics
- QC reports
- Restricted to authorized staff

#### Level 3: Confidential
- Individual sample data
- Geographic identifiers
- Requires specific authorization

#### Level 4: Restricted
- Personally identifiable information (PII)
- Raw demographic data
- Requires explicit approval and audit logging

### 3.2 Classification by Data Element

| Data Element | Classification | Access Level |
|--------------|----------------|--------------|
| Barcode | Confidential | Authorized users |
| Age | Restricted | Approved researchers |
| Sex | Restricted | Approved researchers |
| Province | Confidential | Authorized users |
| Health Zone | Confidential | Authorized users |
| Test Results | Confidential | Authorized users |
| QC Metrics | Internal | All staff |
| Aggregate Statistics | Public | Unrestricted |

---

## 4. Access Control

### 4.1 Access Principles

1. **Least Privilege**: Users receive minimum access required for their role
2. **Need-to-Know**: Access granted only for legitimate business purposes
3. **Separation of Duties**: Critical functions require multiple approvals
4. **Regular Review**: Access rights reviewed quarterly

### 4.2 Access Levels

| Level | Description | Capabilities |
|-------|-------------|--------------|
| Viewer | Read-only access | View dashboards, export summaries |
| Analyst | Standard access | View all data, export detailed reports |
| Editor | Data management | Import data, clear cache, manage QC |
| Admin | Full access | All capabilities, user management |

### 4.3 Access Request Process

1. **Request**: User submits access request form to Data Steward
2. **Approval**: Data Owner approves based on role and need
3. **Provisioning**: Data Custodian configures access
4. **Notification**: User notified of granted access
5. **Documentation**: Request logged in access register

### 4.4 Access Revocation

Access must be revoked when:

- User leaves the organization
- User changes role/project
- Access review identifies unnecessary permissions
- Security incident occurs

---

## 5. Data Quality Standards

### 5.1 Quality Dimensions

| Dimension | Definition | Target |
|-----------|------------|--------|
| Completeness | Required fields populated | > 95% |
| Accuracy | Data matches source | > 99% |
| Consistency | No contradictory values | > 99% |
| Timeliness | Data current and available | < 24h delay |
| Uniqueness | No unintended duplicates | 100% |

### 5.2 Validation Rules

#### Biobank Data
- Barcode format: `KPS-XXXXX` or numeric
- Date format: Valid date within study period
- Age: 0-120 years
- Sex: `M`, `F`, or `Unknown`

#### Test Results
- Cq values: 0-50 or NA
- PP%: 0-200%
- Inhibition %: 0-100%
- Control status: Must pass before sample analysis

### 5.3 Quality Monitoring

- **Daily**: Automated validation checks on data import
- **Weekly**: Data Steward review of quality report
- **Monthly**: Data Owner review of quality trends
- **Quarterly**: Comprehensive quality audit

---

## 6. Audit Trail Requirements

### 6.1 Events to Log

| Event Category | Specific Events |
|----------------|-----------------|
| Authentication | Login, logout, failed attempts |
| Data Access | View, export, search operations |
| Data Modification | Import, edit, delete operations |
| System Changes | Configuration changes, cache operations |
| Admin Actions | User management, access changes |

### 6.2 Log Contents

Each audit log entry must include:

- Timestamp (UTC)
- User identifier
- Action performed
- Data elements affected
- Source IP address
- Success/failure status
- Error details (if applicable)

### 6.3 Log Retention

| Log Type | Retention Period | Storage |
|----------|------------------|---------|
| Authentication | 2 years | Secure server |
| Data Access | 1 year | Secure server |
| Data Modification | 7 years | Secure archive |
| Admin Actions | 7 years | Secure archive |

### 6.4 Audit Log Protection

- Logs stored separately from application data
- Write-once storage (append only)
- Access restricted to Data Custodian
- Regular integrity checks

---

## 7. Data Retention

### 7.1 Retention Schedule

| Data Type | Active Period | Archive Period | Total Retention |
|-----------|---------------|----------------|-----------------|
| Raw test files | Study duration | +5 years | Study + 5 years |
| Processed results | Study duration | +10 years | Study + 10 years |
| Biobank metadata | Indefinite | N/A | Permanent |
| Cache files | Session | None | Session only |
| Audit logs | See Section 6.3 | See Section 6.3 | Per log type |

### 7.2 Archival Process

1. Data Steward identifies data for archival
2. Data Owner approves archival
3. Data Custodian transfers to archive storage
4. Verification of archive integrity
5. Removal from active system (if applicable)

### 7.3 Disposal Process

1. Data Owner authorizes disposal
2. Verification that retention period met
3. Secure deletion (cryptographic erasure preferred)
4. Disposal certificate generated
5. Audit log updated

---

## 8. Change Management

### 8.1 Change Categories

| Category | Description | Approval |
|----------|-------------|----------|
| Standard | Routine, low risk | Data Steward |
| Normal | Planned, moderate risk | Data Owner |
| Emergency | Urgent, high risk | Data Owner + Custodian |

### 8.2 Change Process

1. **Request**: Document proposed change
2. **Review**: Assess impact and risk
3. **Approval**: Obtain required authorization
4. **Test**: Validate in non-production environment
5. **Implement**: Execute change with rollback plan
6. **Verify**: Confirm successful implementation
7. **Document**: Update documentation and close

### 8.3 Change Documentation

Each change must document:

- Description of change
- Business justification
- Risk assessment
- Test results
- Rollback procedure
- Approval signatures

---

## 9. Incident Response

### 9.1 Incident Categories

| Severity | Description | Response Time |
|----------|-------------|---------------|
| Critical | Data breach, system compromise | Immediate |
| High | Data corruption, access violation | < 4 hours |
| Medium | Quality issues, performance | < 24 hours |
| Low | Minor issues, documentation | < 1 week |

### 9.2 Incident Response Steps

1. **Detection**: Identify and confirm incident
2. **Containment**: Limit impact and preserve evidence
3. **Assessment**: Determine scope and severity
4. **Notification**: Alert appropriate stakeholders
5. **Remediation**: Resolve the incident
6. **Recovery**: Restore normal operations
7. **Review**: Post-incident analysis and improvement

### 9.3 Notification Requirements

| Incident Type | Notify |
|---------------|--------|
| Data breach | Data Owner, Legal, Affected parties |
| System compromise | Data Custodian, IT Security |
| Quality issue | Data Steward, Data Owner |
| Access violation | Data Owner, HR (if internal) |

---

## 10. Compliance

### 10.1 Applicable Standards

- Institutional data protection policies
- National health data regulations
- Research ethics committee requirements
- Funding agency data management requirements

### 10.2 Compliance Monitoring

- **Monthly**: Self-assessment checklist
- **Quarterly**: Policy compliance review
- **Annual**: External audit (if required)

### 10.3 Policy Review

This policy is reviewed:

- Annually (minimum)
- After significant incidents
- When regulations change
- When system architecture changes

---

## Appendix A: Access Request Form Template

```
ACCESS REQUEST FORM

Requestor Information:
- Name: _______________________
- Role: _______________________
- Department: _________________
- Email: ______________________

Access Requested:
- Access Level: [ ] Viewer [ ] Analyst [ ] Editor [ ] Admin
- Data Types: [ ] Biobank [ ] MIC [ ] ELISA [ ] iELISA [ ] All
- Justification: _______________________________________________

Approvals:
- Data Steward: ______________ Date: __________
- Data Owner: ______________ Date: __________
```

---

## Appendix B: Data Quality Checklist

- [ ] All required fields populated
- [ ] Barcode format valid
- [ ] Dates within expected range
- [ ] No duplicate records
- [ ] Test results within valid ranges
- [ ] QC controls passed
- [ ] Data linked to biobank correctly

---

## Appendix C: Change Request Template

```
CHANGE REQUEST

Change ID: CR-YYYY-NNN
Requestor: _______________________
Date: __________

Description:
_______________________________________________

Justification:
_______________________________________________

Risk Assessment:
- Impact: [ ] Low [ ] Medium [ ] High
- Likelihood: [ ] Low [ ] Medium [ ] High

Test Plan:
_______________________________________________

Rollback Plan:
_______________________________________________

Approvals:
- Technical Review: ______________ Date: __________
- Data Owner: ______________ Date: __________
```

---

*Document Owner: Data Owner*
*Effective Date: December 2024*
*Review Date: December 2025*

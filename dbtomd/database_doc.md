# Database Documentation: /home/tim/projects/fpc/dbdoc/contacts.db

Database Type: SQLite

Generated: 2026-01-09 22:44:48

## Tables

- [contacts](#table-contacts)
- [sqlite_sequence](#table-sqlite_sequence)
- [contact_phones](#table-contact_phones)
- [contact_emails](#table-contact_emails)

## Table: `contacts`

| Column | Type | Nullable | Default | Key |
|--------|------|----------|---------|-----|
| id | INTEGER | YES |  | PRI |
| first_name | TEXT | NO |  |  |
| last_name | TEXT | NO |  |  |
| address | TEXT | YES |  |  |
| city | TEXT | YES |  |  |
| state | TEXT | YES |  |  |
| postal_code | TEXT | YES |  |  |
| country | TEXT | YES |  |  |
| notes | TEXT | YES |  |  |
| created_at | TEXT | NO | datetime('now') |  |
| updated_at | TEXT | NO | datetime('now') |  |

## Table: `sqlite_sequence`

| Column | Type | Nullable | Default | Key |
|--------|------|----------|---------|-----|
| name |  | YES |  |  |
| seq |  | YES |  |  |

## Table: `contact_phones`

| Column | Type | Nullable | Default | Key |
|--------|------|----------|---------|-----|
| id | INTEGER | YES |  | PRI |
| contact_id | INTEGER | NO |  |  |
| phone | TEXT | NO |  |  |
| phone_type | TEXT | YES |  |  |
| is_primary | INTEGER | NO | 0 |  |

## Table: `contact_emails`

| Column | Type | Nullable | Default | Key |
|--------|------|----------|---------|-----|
| id | INTEGER | YES |  | PRI |
| contact_id | INTEGER | NO |  |  |
| email | TEXT | NO |  |  |
| email_type | TEXT | YES |  |  |
| is_primary | INTEGER | NO | 0 |  |


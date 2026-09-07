# Pure-function tests — no network, no store. Run:
#   Rscript -e 'library(testthat); source("R/fetch.R"); test_dir("tests/testthat")'

test_that("catalog_row fills the full schema and rejects unknown columns", {
  row <- catalog_row(plan_id = "x", title = "T")
  expect_equal(names(row), CATALOG_COLS)
  expect_equal(row$plan_id, "x")
  expect_equal(row$due_date, "")
  expect_error(catalog_row(plan = "typo"), "not catalog columns")
})

test_that("dedupe_filenames disambiguates collisions by doc id", {
  cat <- rbind(
    catalog_row(doc_id = "abc123", filename = "same.pdf"),
    catalog_row(doc_id = "def456", filename = "same.pdf")
  )
  out <- dedupe_filenames(cat)
  expect_equal(out$filename[1], "same.pdf")
  expect_equal(out$filename[2], "same_def456.pdf")
})

test_that("PowerDMS policy codes parse from real naming variants", {
  parse_code <- function(name) {
    if (grepl(POWERDMS_CODE_RE, name)) sub(paste0(POWERDMS_CODE_RE, ".*$"), "\\1", name) else ""
  }
  # Observed in the live manifest, including the double-space variant.
  expect_equal(parse_code("CMP45 Administrative and Financial Sanctions"), "CMP45")
  expect_equal(parse_code("MPCP2007  Complex Case Management"), "MPCP2007")
  expect_equal(parse_code("LAU03 Third Party Liability"), "LAU03")
  # No code -> empty, and the title survives untouched.
  expect_equal(parse_code("Provider Manual Introduction"), "")
  expect_equal(sub(POWERDMS_CODE_RE, "", "MPCP2007  Complex Case Management"),
               "Complex Case Management")
})

test_that("safe_name stays filesystem-safe and readable", {
  expect_equal(safe_name("Workers’ Compensation & Liability!"),
               "Workers_Compensation_Liability")
  expect_equal(nchar(safe_name(strrep("a", 200))), 90)
})

test_that("registry loads, is unique, and refuses unimplemented fetchers", {
  reg <- kb_registry()
  expect_true(length(reg) >= 18)
  expect_true(all(c("alliance", "chpiv", "partnership", "dhcs_apl") %in% names(reg)))
  expect_error(kb_fetchable("nope"), "unknown plan_id")
  expect_error(kb_fetchable("chg"), "not implemented")   # fetcher: none (coverage gap)
  expect_setequal(
    vapply(kb_fetchable(), `[[`, "", "fetcher") |> unique() |> sort(),
    sort(KB_FETCHERS_IMPLEMENTED)
  )
})

test_that("robots parser handles groups, wildcards, and anchors", {
  dis <- parse_robots("User-agent: Google\nDisallow: /g/\n\nUser-agent: *\nDisallow: /*.pdf\nDisallow: /wp-admin/\n")
  expect_setequal(dis, c("/*.pdf", "/wp-admin/"))
  expect_false(robots_allowed("https://x.com/a/b.pdf", dis))
  expect_false(robots_allowed("https://x.com/wp-admin/post", dis))
  expect_true(robots_allowed("https://x.com/a/b.html", dis))
  # empty robots -> everything allowed; $ anchor respected
  expect_true(robots_allowed("https://x.com/anything", parse_robots("")))
  expect_true(robots_allowed("https://x.com/a.pdfx", parse_robots("User-agent: *\nDisallow: /*.pdf$")))
})

test_that("classify_kind separates formulary/form/manual/policy", {
  expect_equal(classify_kind("x/f.pdf", "2026 Provider Manual"), "manual")
  expect_equal(classify_kind("x/f.pdf", "DualConnect Formulary"), "formulary")
  expect_equal(classify_kind("x/f.pdf", "Prior Auth Request Form"), "form")
  expect_equal(classify_kind("x/f.pdf", "Member Handbook"), "member")
  expect_equal(classify_kind("x/f.pdf", "UM01 Authorization Review"), "policy")
  # "formulary" must not be eaten by the \bform\b rule
  expect_equal(classify_kind("x/formulary.pdf", ""), "formulary")
})

test_that("site policy codes parse and titles clean up", {
  expect_equal(extract_code("CO-01 UM Authorization"), "CO-01")
  expect_equal(extract_code("QM38 Quality Committee"), "QM38")
  expect_equal(extract_code("PS-CR03 Credentialing"), "PS-CR03")
  expect_equal(extract_code("2026 Provider Manual"), "")
  expect_equal(extract_code("APL 26-014 Cognitive Health"), "")  # spaced ids are not codes
  expect_equal(doc_title("CO-01 UM Authorization »", "x/y.pdf"), "CO-01 UM Authorization")
  expect_equal(doc_title("Download", "https://x.com/files/UM01_Auth-Review.pdf?v=2"),
               "UM01 Auth Review")
})

test_that("readily slugs derive from the organisation name", {
  # The rule that all three live orgs follow.
  s <- readily_slugs(list(plan_id = "chpiv",
                          name = "Community Health Plan of Imperial Valley"))
  expect_equal(s[1], "community-health-plan-of-imperial-valley")
  expect_true("chpiv" %in% s)
  s2 <- readily_slugs(list(plan_id = "alliance",
                           name = "Central California Alliance for Health"))
  expect_equal(s2[1], "central-california-alliance-for-health")
  # An explicit registry slug always wins over derivation.
  expect_equal(readily_slugs(list(plan_id = "x", name = "Whatever",
                                  readily = list(org = "pinned"))), "pinned")
  # Punctuation and accents must not leak into a URL path.
  expect_false(grepl("[^a-z0-9-]", readily_slugs(list(plan_id = "y",
                     name = "L.A. Care Health Plan"))[1]))
})

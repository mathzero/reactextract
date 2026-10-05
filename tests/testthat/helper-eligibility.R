eligibility_fixture <- function() {
  o <- data.frame(occurrence_id = c("p", "confirm", "x", "y", "age"),
    variable = c("P", "CONFIRM", "X", "Y", "U_AGE"), round_id = "fixture.r01")
  r <- data.frame(routing_rule_id = c("old", "alternative", "follow"), review_state = "approved",
    rule_type = "gate", false_value_kind = "database_missing", false_value = "")
  cc <- data.frame(routing_rule_id = r$routing_rule_id, clause_id = r$routing_rule_id,
    condition_order = 1L, parent_occurrence_id = c("p", "p", "x"),
    operator = c("equals", "equals", "gt"), comparison_values_json = c("[1]", "[2]", "[0]"))
  tt <- data.frame(routing_rule_id = r$routing_rule_id, target_occurrence_id = c("x", "x", "y"))
  cr <- data.frame(routing_rule_id = "new", replaces_rule_id = "old", round_id = "fixture.r01",
    rule_type = "mandatory_gate", review_state = "approved", reviewed_by = "mathzero", review_date = "2000-01-01")
  ct <- data.frame(routing_rule_id = "new", target_occurrence_id = "x")
  cp <- data.frame(routing_rule_id = "new", clause_id = c("adult", "adult", rep("teen", 4)),
    condition_order = c(1:2, 1:4), context_key = c("age", "", "age", "age", "", ""),
    parent_occurrence_id = c("", "p", "", "", "confirm", "p"),
    operator = c("gte", "equals", "gte", "lt", "equals", "equals"),
    comparison_values_json = c("[18]", "[1]", "[13]", "[18]", "[1]", "[1]"))
  # The unrelated old alternative is deliberately permissive: it must not bypass
  # the mandatory full condition. Give it P=1 as well for eligible fixtures.
  cc$operator[2] <- "in"; cc$comparison_values_json[2] <- "[1,2]"
  list(dictionary = list(occurrences = o, routing_rules = r, routing_conditions = cc, routing_targets = tt),
    contract = list(rules = cr, conditions = cp, targets = ct))
}

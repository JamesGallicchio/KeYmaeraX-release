/**
 * Copyright (c) Carnegie Mellon University. CONFIDENTIAL
 * See LICENSE.txt for the conditions of this license.
 */
package edu.cmu.cs.ls.keymaerax.macros

/** UI display information on how to render an axiom, rule, or tactic application */
sealed trait DisplayInfo {
  /** how to render an axiom/rule/tactic name in the UI. May contain unicode or html. */
  def name: String
  /** how to render an axiom/rule/tactic name in ASCII plain text */
  def asciiName: String
}
/** Render as a UI name and a plain text ASCII name. */
case class SimpleDisplayInfo(override val name: String, override val asciiName: String) extends DisplayInfo
/** Render a rule with a name as a conclusion and list of premises. */
case class RuleDisplayInfo(names: SimpleDisplayInfo, conclusion: SequentDisplay, premises:List[SequentDisplay]) extends DisplayInfo {
  override def name = names.name
  override def asciiName = names.asciiName
}
/** Render a sequent as a list of antecedent UI strings and a list of succedent UI strings.
 * @param isClosed true to indicate that this sequent is closed so (*) star. */
case class SequentDisplay(ante: List[String], succ: List[String], isClosed: Boolean = false)
/** Render an axiom with a name as a UI string for the formula. */
case class AxiomDisplayInfo(names: SimpleDisplayInfo, displayFormula: String) extends DisplayInfo {
  override def name = names.name
  override def asciiName = names.asciiName

}
object AxiomDisplayInfo {
  /**
    * Elaborate the display formula from a raw unicode string to html.
    */
  def render(names: SimpleDisplayInfo, displayFormula: String): AxiomDisplayInfo = AxiomDisplayInfo(
    names,
    displayFormula.
      replaceAllLiterally("<","&lt;").
      replaceAllLiterally(">","&gt;").
      replaceAll("&lt;(/?(\\w+))&gt;", "<$1>"). // undo escaping HTML tags
      replaceFirst("__", "<span class=\"k4-axiom-key\">").
      replaceFirst("__", "</span>")
  )
}

/** Render an axiom that has a name and a UI string formula but needs a list of inputs filled in first. */
case class InputAxiomDisplayInfo(names: SimpleDisplayInfo, displayFormula: String, input: List[ArgInfo]) extends DisplayInfo {
  override def name = names.name
  override def asciiName = names.asciiName
}
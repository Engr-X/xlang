/*
 * Copyright (c) 2026 Di Wang
 * SPDX-License-Identifier: MIT
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */

package com.diwang.icontoolkit

import com.github.weisj.jsvg.SVGDocument
import com.github.weisj.jsvg.parser.LoaderContext
import com.github.weisj.jsvg.parser.SVGLoader
import com.github.weisj.jsvg.view.FloatSize
import com.github.weisj.jsvg.view.ViewBox

import org.w3c.dom.Document
import org.w3c.dom.Element
import org.w3c.dom.Node
import org.w3c.dom.NodeList

import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.StringWriter
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

import javax.imageio.ImageIO
import javax.swing.ImageIcon
import javax.xml.parsers.DocumentBuilder
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.OutputKeys
import javax.xml.transform.Transformer
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult

/**
 * This class provide interface for converting svg to png, use recursion to replace each imported svg.
 * Also it support generate icon for exe file in windows.
 *
 * @author Di Wang
 * @since Alpha-1.1.0
 */

class SVGraphics(svgFile: Path)
{
    companion object
    {
        private val SVG_LOADER = SVGLoader()

        private val TRANSFORMER: Transformer = TransformerFactory.newInstance().newTransformer()

        private const val XLINK_NS: String = "http://www.w3.org/1999/xlink"

        /**
         * Parse an SVG/XML file into a DOM document.
         *
         * @param path	SVG/XML file path.
         * @return		Parsed DOM [Document].
         */
        private fun parseXml(path: Path): Document
        {
            val builder: DocumentBuilder = DocumentBuilderFactory.newInstance().apply {
                isNamespaceAware = true
            }.newDocumentBuilder()

            return builder.parse(path.toFile())
        }

        /**
         * Convert raw SVG text to a Swing [ImageIcon] at the given size.
         *
         * @param svgCode						Raw SVG markup.
         * @param width						    Target icon width in pixels, must be `> 0`.
         * @param height						Target icon height in pixels, must be `> 0`.
         * @return								Rendered [ImageIcon].
         * @throws IllegalArgumentException	    When width/height are invalid.
         */
        @SuppressWarnings("unused")
        fun svgStringToIcon(svgCode: String, width: Int, height: Int): ImageIcon
        {
            require(width > 0) { "width must be > 0" }
            require(height > 0) { "height must be > 0" }

            val svg: SVGDocument = loadSvgFromString(svgCode, URI.create("about:blank"))
            val image: BufferedImage = renderSvgToImage(svg, width, height)
            return ImageIcon(image)
        }

        /**
         * Load an [SVGDocument] from SVG text.
         *
         * @param svgCode	Raw SVG markup.
         * @param baseUri	Base URI used for resolving relative resources.
         * @return			Parsed [SVGDocument].
         */
        private fun loadSvgFromString(svgCode: String, baseUri: URI): SVGDocument
        {
            val svgBytes: ByteArray = svgCode.toByteArray(StandardCharsets.UTF_8)
            return ByteArrayInputStream(svgBytes).use { input ->
                SVG_LOADER.load(input, baseUri, LoaderContext.createDefault())
            } ?: error("Cannot load SVG")
        }

        /**
         * Render an [SVGDocument] to an ARGB [BufferedImage].
         *
         * @param svg		Input SVG document.
         * @param width	    Target width in pixels. Use `-1` to auto-calculate.
         * @param height	Target height in pixels. Use `-1` to auto-calculate.
         * @return			Rendered ARGB image.
         */
        @SuppressWarnings("unused")
        private fun renderSvgToImage(svg: SVGDocument, width: Int = -1, height: Int = -1): BufferedImage
        {
            val size: FloatSize = svg.size()
            var sourceWidth: Double = size.width.toDouble()
            var sourceHeight: Double = size.height.toDouble()
            val vb: ViewBox = svg.viewBox()
            sourceWidth = maxOf(sourceWidth, vb.width.toDouble(), -1.0).takeIf { it > 0.0 } ?: 256.0
            sourceHeight = maxOf(sourceHeight, vb.height.toDouble(), -1.0).takeIf { it > 0.0 } ?: 256.0

            val targetWidth: Int = when
            {
                width > 0 -> width
                height > 0 -> maxOf(1, (height * sourceWidth / sourceHeight).toInt())
                else -> maxOf(1, sourceWidth.toInt())
            }

            val targetHeight: Int = when
            {
                height > 0 -> height
                width > 0 -> maxOf(1, (width * sourceHeight / sourceWidth).toInt())
                else -> maxOf(1, sourceHeight.toInt())
            }

            val image = BufferedImage(targetWidth, targetHeight, BufferedImage.TYPE_INT_ARGB)
            val g: Graphics2D = image.createGraphics()

            try
            {
                g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
                g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
                g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
                g.scale(targetWidth / sourceWidth, targetHeight / sourceHeight)
                svg.render(null, g)
            }
            finally
            {
                g.dispose()
            }

            return image
        }
    }

    private val rootFile: Path = svgFile.toAbsolutePath().normalize()
    private val mainDoc: Document = parseXml(this.rootFile)

    /**
     * Convenience constructor from string path.
     *
     * @param path	SVG file path string.
     */
    constructor(path: String) : this(Path.of(path))


    /**
     * Inline nested `<image href="...svg">` references into the current DOM.
     *
     * @return	Current [SVGraphics] instance for chaining.
     */
    fun inline(): SVGraphics
    {
        val baseDir: Path = this.rootFile.parent ?: Path.of(".").toAbsolutePath().normalize()
        this.inlineImages(this.mainDoc, this.mainDoc.documentElement, baseDir, mutableSetOf(this.rootFile))
        return this
    }


    /**
     * Render and save the current SVG as PNG.
     *
     * @param path		Output PNG path.
     * @param width	    Target width in pixels. Use `-1` to auto-calculate.
     * @param height	Target height in pixels. Use `-1` to auto-calculate.
     */
    fun saveAsPng(path: Path, width: Int = -1, height: Int = -1)
    {
        val svg: SVGDocument = this.renderToSvgDocument()
        val image: BufferedImage = renderSvgToImage(svg, width, height)

        val outputPath: Path = path.toAbsolutePath().normalize()
        val parent: Path? = outputPath.parent

        if (parent != null)
            Files.createDirectories(parent)

        val ok: Boolean = ImageIO.write(image, "png", outputPath.toFile())
        check(ok) { "No ImageIO writer found for PNG format." }
    }

    /**
     * Render and save the current SVG as ICO (single PNG-based frame).
     *
     * @param path							Output ICO path.
     * @param width						    Icon width in pixels, range `1..256`.
     * @param height						Icon height in pixels, range `1..256`.
     * @throws IllegalArgumentException 	When width/height are outside ICO limits.
     */
    fun saveAsIco(path: Path, width: Int = 256, height: Int = 256)
    {
        require(width in 1..256) { "ICO width must be in 1..256" }
        require(height in 1..256) { "ICO height must be in 1..256" }

        val svg: SVGDocument = this.renderToSvgDocument()
        val image: BufferedImage = renderSvgToImage(svg, width, height)
        val pngBytes: ByteArray = bufferedImageToPng(image)
        val icoBytes: ByteArray = wrapPngAsSingleFrameIco(pngBytes, width, height)

        val outputPath: Path = path.toAbsolutePath().normalize()
        val parent: Path? = outputPath.parent
        if (parent != null)
            Files.createDirectories(parent)

        Files.write(outputPath, icoBytes)
    }

    /**
     * Render the current SVG into a Swing [ImageIcon].
     *
     * @param width	    Target icon width in pixels. Use `-1` to auto-calculate.
     * @param height	Target icon height in pixels. Use `-1` to auto-calculate.
     * @return			Rendered [ImageIcon].
     */
    @SuppressWarnings("unused")
    fun saveAsIcon(width: Int = -1, height: Int = -1): ImageIcon
    {
        val svg: SVGDocument = this.renderToSvgDocument()
        val image: BufferedImage = renderSvgToImage(svg, width, height)
        return ImageIcon(image)
    }

    /**
     * Convert current DOM back to `jsvg`'s [SVGDocument] for rendering.
     *
     * @return	Renderable [SVGDocument].
     */
    private fun renderToSvgDocument(): SVGDocument =
        loadSvgFromString(this.toRawCode(), this.rootFile.toUri())

    /**
     * Recursively replace `<image>` nodes by importing child SVG content as `<g>`.
     *
     * @param ownerDoc		Destination document that receives imported nodes.
     * @param root			Current element subtree root.
     * @param baseDir		Base directory used to resolve relative href paths.
     * @param includeStack	Stack of already included files for cycle detection.
     */
    private fun inlineImages(ownerDoc: Document, root: Element, baseDir: Path, includeStack: MutableSet<Path>)
    {
        for (image in collectImageElements(root))
        {
            val href: String = image.getAttribute("href")
                .ifEmpty { image.getAttributeNS(XLINK_NS, "href") }
                .trim()

            if (href.isBlank())
                continue

            val childPath: Path = this.resolveHrefToPath(baseDir, href) ?: continue

            if (!Files.exists(childPath))
                throw IllegalArgumentException("Referenced SVG not found: $childPath (href=\"$href\")")

            if (!includeStack.add(childPath))
                throw IllegalArgumentException("Detected cyclic <image> reference involving: $childPath")

            val childDoc: Document = parseXml(childPath)
            val childRoot: Element = childDoc.documentElement

            val childBaseDir: Path = childPath.parent ?: baseDir
            this.inlineImages(childDoc, childRoot, childBaseDir, includeStack)
            includeStack.remove(childPath)

            val group: Element = ownerDoc.createElement("g")

            this.copyUsefulAttributes(image, group)

            val transform: String = this.buildTransform(image, childRoot)

            if (transform.isNotBlank())
                group.setAttribute("transform", transform)

            val children: NodeList = childRoot.childNodes

            for (i in 0 until children.length)
            {
                val child: Node = children.item(i)
                val imported: Node = ownerDoc.importNode(child, true)
                group.appendChild(imported)
            }

            image.parentNode.replaceChild(group, image)
        }
    }

    /**
     * Resolve local `href`/`file:` URI to an absolute path.
     *
     * @param baseDir	Base directory for relative path resolution.
     * @param href		Raw href attribute value.
     * @return			Resolved path, or `null` for data/remote references.
     */
    private fun resolveHrefToPath(baseDir: Path, href: String): Path?
    {
        if (href.startsWith("data:", ignoreCase = true))
            return null

        if (href.startsWith("file:", ignoreCase = true))
        {
            return runCatching { Path.of(URI(href)) }
                .getOrElse { Path.of(href.removePrefix("file:")) }
                .let { if (it.isAbsolute) it else baseDir.resolve(it) }
                .normalize()
        }

        // Skip remote references; this inliner only handles local files.
        if (Regex("^[a-zA-Z][a-zA-Z0-9+.-]*:").containsMatchIn(href))
            return null

        return baseDir.resolve(href).normalize()
    }

    /**
     * Collect all `<image>` elements from the given element subtree.
     *
     * @param root	Subtree root.
     * @return		Mutable list of `<image>` elements.
     */
    private fun collectImageElements(root: Element): MutableList<Element>
    {
        val result: MutableList<Element> = mutableListOf()
        val nodes: NodeList = root.getElementsByTagName("image")

        for (i in 0 until nodes.length)
        {
            val node: Node = nodes.item(i)

            if (node.nodeType == Node.ELEMENT_NODE)
                result.add(node as Element)
        }

        return result
    }

    /**
     * Copy style-related attributes when replacing an `<image>` with a `<g>`.
     *
     * @param from	Source element.
     * @param to	Destination element.
     */
    private fun copyUsefulAttributes(from: Element, to: Element)
    {
        val keep: MutableList<String> = mutableListOf(
            "id", "class", "style", "filter",
            "opacity", "clip-path", "mask")

        for (name: String in keep)
        {
            if (from.hasAttribute(name))
                to.setAttribute(name, from.getAttribute(name))
        }
    }

    /**
     * Build transform so imported child SVG matches original `<image>` placement.
     *
     * @param image		    Original `<image>` element.
     * @param childRoot	    Root element of referenced child SVG.
     * @return				SVG `transform` expression.
     */
    private fun buildTransform(image: Element, childRoot: Element): String
    {
        val x: Double = image.getDouble("x", 0.0)
        val y: Double = image.getDouble("y", 0.0)
        val width: Double = image.getDouble("width", 0.0)
        val height: Double = image.getDouble("height", 0.0)

        val viewBox: String = childRoot.getAttribute("viewBox")

        if (viewBox.isBlank() || width == 0.0 || height == 0.0)
            return "translate($x $y)"

        val parts = viewBox.trim().split(Regex("[,\\s]+")).map { it.toDouble() }

        if (parts.size != 4)
            return "translate($x $y)"

        val (minX: Double, minY: Double) = parts[0] to parts[1]
        val (vbWidth: Double, vbHeight: Double) = parts[2] to parts[3]
        val scale: Double = minOf(width / vbWidth, height / vbHeight)

        // preserveAspectRatio="xMidYMid meet"
        val alignX: Double = (width - vbWidth * scale) / 2.0
        val alignY: Double = (height - vbHeight * scale) / 2.0

        return "translate(${x + alignX} ${y + alignY}) scale($scale) translate(${-minX} ${-minY})"
    }

    /**
     * Parse an element attribute as [Double] with fallback.
     *
     * @param name		Attribute name.
     * @param default	Fallback value when attribute is absent or invalid.
     * @return			Parsed attribute value or fallback.
     */
    private fun Element.getDouble(name: String, default: Double): Double =
        this.getAttribute(name).toDoubleOrNull() ?: default


    /**
     * Serialize current DOM to SVG text.
     *
     * @return	Serialized SVG code.
     */
    private fun toRawCode(): String
    {
        val writer = StringWriter()

        TRANSFORMER.apply {
            this.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no")
            this.setOutputProperty(OutputKeys.INDENT, "yes")
            this.transform(DOMSource(mainDoc), StreamResult(writer))
        }

        return writer.toString()
    }


    /**
     * String view of current SVG DOM.
     *
     * @return	Serialized SVG code.
     */
    override fun toString(): String = this.toRawCode()

    /**
     * Encode ARGB image to PNG bytes.
     *
     * @param image	    Source ARGB image.
     * @return			PNG bytes.
     */
    private fun bufferedImageToPng(image: BufferedImage): ByteArray
    {
        val out = ByteArrayOutputStream()
        val ok: Boolean = ImageIO.write(image, "png", out)
        check(ok) { "No ImageIO writer found for PNG format." }
        return out.toByteArray()
    }

    /**
     * Wrap PNG bytes into a minimal single-frame ICO container.
     *
     * @param pngData	PNG payload bytes.
     * @param width	    Icon width in pixels.
     * @param height	Icon height in pixels.
     * @return			Full ICO bytes.
     */
    private fun wrapPngAsSingleFrameIco(pngData: ByteArray, width: Int, height: Int): ByteArray
    {
        val out = ByteArrayOutputStream(22 + pngData.size)

        // ICONDIR: reserved(2)=0, type(2)=1(icon), count(2)=1
        writeLe16(out, 0)
        writeLe16(out, 1)
        writeLe16(out, 1)

        // ICONDIRENTRY (16 bytes)
        out.write(if (width == 256) 0 else width)    // bWidth
        out.write(if (height == 256) 0 else height)  // bHeight
        out.write(0)                                  // bColorCount
        out.write(0)                                  // bReserved
        writeLe16(out, 1)                             // wPlanes
        writeLe16(out, 32)                            // wBitCount
        writeLe32(out, pngData.size)                  // dwBytesInRes
        writeLe32(out, 22)                            // dwImageOffset

        out.write(pngData)
        return out.toByteArray()
    }

    /**
     * Write a 16-bit little-endian integer.
     *
     * @param out		Target output stream.
     * @param value	    Integer value to write.
     */
    private fun writeLe16(out: ByteArrayOutputStream, value: Int)
    {
        out.write(value and 0xFF)
        out.write((value ushr 8) and 0xFF)
    }

    /**
     * Write a 32-bit little-endian integer.
     *
     * @param out		Target output stream.
     * @param value	    Integer value to write.
     */
    private fun writeLe32(out: ByteArrayOutputStream, value: Int)
    {
        out.write(value and 0xFF)
        out.write((value ushr 8) and 0xFF)
        out.write((value ushr 16) and 0xFF)
        out.write((value ushr 24) and 0xFF)
    }
}

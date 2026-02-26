import type { Metadata } from 'next'
import '@/styles/globals.css'
import Navbar from '@/components/navigation/Navbar'
import Footer from '@/components/navigation/Footer'
import ScrollProgress from '@/components/navigation/ScrollProgress'
import SmoothScroll from '@/components/providers/SmoothScroll'
import CustomCursor from '@/components/ui/CustomCursor'
import NoiseOverlay from '@/components/ui/NoiseOverlay'
import PageTransition from '@/components/providers/PageTransition'
import { SITE_CONFIG } from '@/lib/constants'

export const metadata: Metadata = {
  metadataBase: new URL(SITE_CONFIG.url),
  title: {
    default: SITE_CONFIG.title,
    template: '%s | Evan Roden',
  },
  description: SITE_CONFIG.description,
  openGraph: {
    title: SITE_CONFIG.title,
    description: SITE_CONFIG.description,
    url: SITE_CONFIG.url,
    siteName: SITE_CONFIG.name,
    locale: 'en_US',
    type: 'website',
  },
  twitter: {
    card: 'summary_large_image',
    title: SITE_CONFIG.title,
    description: SITE_CONFIG.description,
  },
  robots: {
    index: true,
    follow: true,
  },
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en">
      <body className="font-sans antialiased bg-slate-950 text-white">
        <SmoothScroll>
          <NoiseOverlay />
          <CustomCursor />
          <ScrollProgress />
          <Navbar />
          <main className="min-h-screen">
            <PageTransition>
              {children}
            </PageTransition>
          </main>
          <Footer />
        </SmoothScroll>
      </body>
    </html>
  )
}

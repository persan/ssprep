package net.sourceforge.ssprep;


import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;

public class NewIDLProjectGroupWizard extends Wizard implements INewWizard {
	private NewIDLProjectGroupWizardPage page;

	public NewIDLProjectGroupWizard() {
		super.setWindowTitle("New IDL Project Group");
	}

	@Override
	public void addPages() {
		this.page = new NewIDLProjectGroupWizardPage("New IDL Project Group");
		super.addPage(this.page);
	}

	@Override
	public boolean performFinish() {
		final String   projectName   = this.page.getProjectName();
		final String   templateName  = this.page.getTemplateName();
		final String   workspacePath = ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString();

		// Create runnable object
		IRunnableWithProgress operation = new IRunnableWithProgress() {
			public void run(IProgressMonitor monitor) throws InvocationTargetException {
				try {
					doFinish(projectName, templateName, workspacePath, monitor);
				}
				catch (RuntimeException e) {
					throw new InvocationTargetException(e);
				}
				finally {
					monitor.done();
				}
			}
		};
		// Execute runnable object to create directories
		try {
			super.getContainer().run(true, false, operation);
			Thread.sleep(1000);
		}
		catch (InterruptedException e) {
			return false;
		}
		catch (InvocationTargetException e) {
			Throwable realException = e.getTargetException();
			MessageDialog.openError(super.getShell(), "Error", realException.getMessage());
			return false;
		}

		// Create projects
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		File workspaceDirectory = new File(root.getLocation().toOSString());
		if (workspaceDirectory.exists() &&  workspaceDirectory.canRead()) {
			// Find project directories created by 'ssprep'
			File[] projectDirectories = workspaceDirectory.listFiles(new FilenameFilter() {
				@Override
				public boolean accept(File file, String name) {
					return name.startsWith(projectName);
				}
			});
			for (File projectDirectory : projectDirectories) {
				File[] projectFiles = projectDirectory.listFiles(new FilenameFilter() {
					@Override
					public boolean accept(File file, String name) {
						return name.equals(".project");
					}
				});
				try {
					for (File projectFile : projectFiles) {
						final String path = projectFile.getParentFile().getName();
						IProject project = root.getProject(path);
						project.create(null);
						project.open(null);
					}
				} catch (CoreException e) {
					MessageDialog.openError(super.getShell(), "Error", e.getMessage());
					return false;
				}
			}
		}
		return true;
	}

	private void doFinish(String projectName, String templateName, String workspacePath, IProgressMonitor monitor) throws RuntimeException {
        String[] commandArray = new String[4];
        commandArray[0] = "ssprep";
        commandArray[1] = templateName;
        commandArray[2] = "-Dproject=" + projectName;
        commandArray[3] = "-o=" + workspacePath;
        Runtime rt = Runtime.getRuntime();
        try {
        	monitor.beginTask("Creating IDL Project group", 1);
            rt.exec(commandArray);
            monitor.worked(1);
        } catch (IOException e) {
            throw new RuntimeException(e.getMessage());
        }
	}

	@Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
	}
}

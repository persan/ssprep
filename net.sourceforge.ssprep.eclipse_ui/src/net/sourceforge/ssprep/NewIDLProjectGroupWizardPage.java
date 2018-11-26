package net.sourceforge.ssprep;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

public class NewIDLProjectGroupWizardPage extends WizardPage {
	private static final String TEMPLATE = "com.saabgroup.tmplt.ndds-gen";
	
	private Text    projectNameTextBox;
	private boolean isEditingStarted;
	private boolean isProjectNameValid;

	protected NewIDLProjectGroupWizardPage(String pageName) {
		super(pageName);
		super.setTitle("Create a new IDL project group");
		super.setDescription(
				"Enter project name. Ensure that 'ssprep' exists in the path, and that " +
				"the template '" + TEMPLATE + "' is defined and visible to 'ssprep'.");
		super.setImageDescriptor(Activator.getImageDescriptor("icons/wizard.png"));
		this.isEditingStarted = false;
		this.isProjectNameValid = false;
	}

	@Override
	public void createControl(Composite parent) {
		Composite container = new Composite(parent, SWT.NULL);
		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		container.setLayout(layout);
		
		GridData gd1 = new GridData(SWT.FILL, SWT.TOP, true, false);
		Label label = new Label(container, SWT.NULL);
		label.setText("Project name:");
		
		this.projectNameTextBox = new Text(container, SWT.BORDER | SWT.SINGLE);
		this.projectNameTextBox.setLayoutData(gd1);
		this.projectNameTextBox.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				NewIDLProjectGroupWizardPage.this.isEditingStarted = true;
				validateProjectName();
			}
		});
		validateProjectName();
		super.setControl(container);
	}

	@Override
	public boolean isPageComplete() {
		setErrorMessage(null);
		if (!this.isProjectNameValid && this.isEditingStarted) {
			setErrorMessage("Invalid project name");
		}
		return this.isProjectNameValid;
	}

	public String getProjectName() {
		return this.projectNameTextBox.getText();
	}
	
	public String getTemplateName() {
		return TEMPLATE;
	}
	
	@Override
	public boolean canFlipToNextPage() {
		return false;
	}

	private void validateProjectName() {
		IStatus result = ResourcesPlugin.getWorkspace().validateName(this.projectNameTextBox.getText(), IResource.PROJECT);
		this.isProjectNameValid = result.isOK();
		super.setPageComplete(isPageComplete());
	}
}
